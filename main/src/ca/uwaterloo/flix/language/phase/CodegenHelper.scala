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

import ca.uwaterloo.flix.api
import ca.uwaterloo.flix.api.cell._
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Type => _, _}

/**
  * This object holds methods used by different phases of code generations
  */
object CodegenHelper {

  // Prefix of Enums
  val fixedEnumPrefix = List("ca", "waterloo", "flix", "enums")

  // Prefix of Tuples
  val fixedTuplePrefix = List("ca", "waterloo", "flix", "tuples")

  // Version of Java
  val JavaVersion = V1_8

  /**
    * Wrapper around similar types. This is used to group types which are similar. For example, all non primitive types
    * need to be represented using an object if they are a field of an enum so we can just create one enum class with an
    * object field. We group all of them using `WrappedNonPrimitives(..)`.
    */
  sealed trait WrappedType

  /**
    * A wrapper around a primitive type.
    *
    * @param prim The wrapped primitive
    */
  case class WrappedPrimitive(prim: Type) extends WrappedType

  /**
    * A wrapper around types which needs to be represented by an object which is any type which cannot be represented by
    * a primitive. Note that `underlying` includes all the wrapped types when it is created in TupleGen or EnumGen but
    * in CodeGen and LoadbyteCode it may not include all the wrapped types.
    *
    * @param underlying underlying types
    */
  case class WrappedNonPrimitives(underlying: Set[Type]) extends WrappedType

  /**
    * Wrapper around qualified name of a class
    * Note that QualName represent a unique address to a class, so two QualName instances with the same address are
    * the same.
    */
  sealed trait QualName {
    /**
      * A list representing the qualified name of a class
      */
    def ref: List[String]

    /**
      * Returning hashCode of the `ref` since QualName is just a wrapper around it
      *
      * @return hashCode
      */
    override def hashCode(): Int = ref.hashCode()

    /**
      * Override equality to just check for equality of references
      *
      * @param obj objects to be compared to each other
      * @return `true` if they are equal
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case q: QualName => ref == q.ref
      case _ => false
    }
  }

  /**
    * Simple implementation of `QualName` used to generate classes containing flix functions and functional interfaces
    */
  case class FlixClassName(ref: List[String]) extends QualName

  /**
    * Qualified name of an enum class
    *
    * @param sym symbol of the enum
    * @param tag tag of the enum
    * @param tpe tpe of the field of the enum
    */
  case class EnumClassName(sym: EnumSym, tag: String, tpe: WrappedType) extends QualName {
    val ref: List[String] = tpe match {
      case WrappedPrimitive(t) => CodegenHelper.fixedEnumPrefix ::: sym.namespace ::: List(sym.name, typeSpecifier(t), tag)
      case _ => CodegenHelper.fixedEnumPrefix ::: sym.namespace ::: List(sym.name, "object", tag)
    }
  }

  /**
    * Qualified name of an enum interface
    *
    * @param sym symbol of the enum
    */
  case class EnumInterfName(sym: EnumSym) extends QualName {
    val ref: List[String] = CodegenHelper.fixedEnumPrefix ::: sym.namespace ::: List(sym.name, "EnumInterface")
  }

  /**
    * Qualified name of a tuple class
    *
    * @param fields fields of the tuple
    */
  case class TupleClassName(fields: List[WrappedType]) extends QualName {
    val ref: List[String] = fixedTuplePrefix ::: fields.map {
      case WrappedPrimitive(tpe) => typeSpecifier(tpe)
      case WrappedNonPrimitives(_) => "object"
    } ::: List("Tuple")
  }

  /**
    * Base filename of a file specified by the given qualified name
    *
    * @param qualName Qualified name of the file
    */
  def baseFileName(qualName: QualName): String = qualName match {
    case FlixClassName(ref) => s"${ref.last}.flix"
    case EnumClassName(sym, _, _) => sym.loc.source.format
    case EnumInterfName(sym) => sym.loc.source.format
    case _ => throw InternalCompilerException(s"QualName $qualName does not have a base file")
  }

  /**
    * This method returns a string which uniquely specifies the give type. If the type can be represented by a primitive,
    * then the name of that primitive is returned, otherwise if the type has to be represented by an object then string
    * `object` is return
    *
    * @param tpe type to be specified
    */
  def typeSpecifier(tpe: Type): String = {
    val objectStr = "object"
    tpe match {
      case Type.Var(x, k) => throw InternalCompilerException(s"Unexpected type: $tpe")
      case Type.Unit => objectStr
      case Type.Bool => "Bool"
      case Type.Char => "Char"
      case Type.Float32 => "Float32"
      case Type.Float64 => "Float64"
      case Type.Int8 => "Int8"
      case Type.Int16 => "Int16"
      case Type.Int32 => "Int32"
      case Type.Int64 => "Int64"
      case Type.BigInt => objectStr
      case Type.Str => objectStr
      case Type.Native => objectStr

      case _ if tpe.isArrow => objectStr
      case _ if tpe.isEnum => objectStr
      case _ if tpe.isRef => objectStr
      case _ if tpe.isTuple => objectStr
    }
  }

  /**
    * If a type is primitive, we wrap it in `WrappedPrimitive`, else if the type requires an object to be represented, we
    * wrap it inside `WrappedNonPrimitives`
    *
    * @param tpe the type to be transformed
    */
  def typeToWrappedType(tpe: Type): WrappedType = tpe match {
    case t if isPrimitive(t) => WrappedPrimitive(t)
    case _ => WrappedNonPrimitives(Set(tpe))
  }

  /**
    * Generates a field for the class with with name `name`, with descriptor `descriptor` using `visitor`. If `isStatic = true`
    * then the field is static, otherwise the field will be non-static.
    * For example calling this method with name = `field01`, descriptor = `I`, isStatic = `false` and isPrivate = `true`
    * creates the following field:
    *
    * private int field01;
    *
    * calling this method with name = `value`, descriptor = `java/lang/Object`, isStatic = `false` and isPrivate = `true`
    * creates the following:
    *
    * private Object value;
    *
    * calling this method with name = `unitInstance`, descriptor = `ca/waterloo/flix/enums/List/object/Nil`, `isStatic = true`
    * and isPrivate = `false` generates the following:
    *
    * public static Nil unitInstance;
    *
    * @param visitor    class visitor
    * @param name       name of the field
    * @param descriptor descriptor of field
    * @param isStatic   if this is true the the field is static
    * @param isPrivate  if this is set then the field is private
    */
  def compileField(visitor: ClassWriter, name: String, descriptor: String, isStatic: Boolean, isPrivate: Boolean): Unit = {
    val visibility =
      if (isPrivate) {
        ACC_PRIVATE
      } else {
        ACC_PUBLIC
      }

    val fieldType =
      if (isStatic) {
        ACC_STATIC
      } else {
        0
      }

    val field = visitor.visitField(visibility + fieldType, name, descriptor, null, null)
    field.visitEnd()
  }

  /**
    * Generate the `methodName` method for fetching the `fieldName` field of the class.
    * `name` is name of the class and `descriptor` is type of the `fieldName` field.
    * For example, `Val[Char]` has following `getValue()`method:
    *
    * public final char getValue() {
    * return this.value;
    * }
    *
    * @param visitor    class visitor
    * @param qualName   Qualified name of the class
    * @param fieldName  name of the field
    * @param methodName method name of getter of `fieldName`
    * @param iReturn    opcode for returning the value of the field
    */
  def compileGetFieldMethod(visitor: ClassWriter, qualName: QualName, descriptor: String, fieldName: String,
                            methodName: String, iReturn: Int): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, s"()$descriptor", null, null)

    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, decorate(qualName), fieldName, descriptor)
    method.visitInsn(iReturn)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `methodName` method for setting the `fieldName` field of the class.
    * `name` is name of the class and `descriptor` is type of the `fieldName` field.
    * For example, the class of `Tuple[Int32, Int32]` has the following `setField0` method:
    *
    * public final void setField0(int var) {
    *   this.field0 = var;
    * }
    *
    * @param visitor    class visitor
    * @param qualName   Qualified name of the class
    * @param fieldName  name of the field
    * @param methodName method name of getter of `fieldName`
    * @param iLoad      opcode for loading the single parameter of the method
    */
  def compileSetFieldMethod(visitor: ClassWriter, qualName: QualName, descriptor: String, fieldName: String,
                            methodName: String, iLoad: Int): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, s"($descriptor)V", null, null)

    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(iLoad, 1)
    method.visitFieldInsn(PUTFIELD, decorate(qualName), fieldName, descriptor)
    method.visitInsn(RETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Returns the load instruction corresponding to the `fType`, if it is `None` then the type can be represented by an object
    *
    * @param fType type
    * @return A load instruction
    */
  def getReturnInsn(fType: WrappedType): Int = fType match {
    case WrappedPrimitive(Type.Bool) | WrappedPrimitive(Type.Char) | WrappedPrimitive(Type.Int8) | WrappedPrimitive(Type.Int16) |
         WrappedPrimitive(Type.Int32) => IRETURN
    case WrappedPrimitive(Type.Int64) => LRETURN
    case WrappedPrimitive(Type.Float32) => FRETURN
    case WrappedPrimitive(Type.Float64) => DRETURN
    case _ => ARETURN
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    *
    * @param tpe Wrapped type to be loaded
    * @return Appropriate load instruction for the given type
    */
  def getLoadInstruction(tpe: WrappedType): Int = tpe match {
    case WrappedPrimitive(Type.Bool) => ILOAD
    case WrappedPrimitive(Type.Char) => ILOAD
    case WrappedPrimitive(Type.Int8) => ILOAD
    case WrappedPrimitive(Type.Int16) => ILOAD
    case WrappedPrimitive(Type.Int32) => ILOAD
    case WrappedPrimitive(Type.Int64) => LLOAD
    case WrappedPrimitive(Type.Float32) => FLOAD
    case WrappedPrimitive(Type.Float64) => DLOAD
    case _ => ALOAD
  }

  /**
    * This function is called to compare two values on top of the stack with the type `tpe`.
    * We will pick the appropriate comparison between the two values, if they are not equal, we
    * jump to the `label`, otherwise we continue with the current control flow.
    *
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe    Wrapped type of the value on top of the stack
    * @param label  label in case that values on top of the stack are not equal
    */
  def branchIfNotEqual(method: MethodVisitor, tpe: WrappedType, label: Label): Unit = {
    val clazz = Constants.objectClass
    val objectEqualsMethod = clazz.getMethod("equals", clazz)

    tpe match {
      case WrappedPrimitive(Type.Bool) | WrappedPrimitive(Type.Char) | WrappedPrimitive(Type.Int8) | WrappedPrimitive(Type.Int16) |
           WrappedPrimitive(Type.Int32) =>
        method.visitJumpInsn(IF_ICMPNE, label)
      case WrappedPrimitive(Type.Int64) =>
        method.visitInsn(LCMP)
        method.visitJumpInsn(IFNE, label)
      case WrappedPrimitive(Type.Float32) =>
        method.visitInsn(FCMPG)
        method.visitJumpInsn(IFNE, label)
      case WrappedPrimitive(Type.Float64) =>
        method.visitInsn(DCMPG)
        method.visitJumpInsn(IFNE, label)
      case _ =>
        method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), objectEqualsMethod.getName,
          asm.Type.getMethodDescriptor(objectEqualsMethod), false)
        method.visitJumpInsn(IFEQ, label)
    }
  }

  /**
    * Returns true if the case has a unit field, which means the case can be a singleton. It returns false otherwise.
    *
    * @param cs Enum Case
    */
  def isSingletonEnum(cs: ExecutableAst.Case): Boolean = {
    cs.tpe == Type.Unit
  }

  /**
    * This method is used to represent the value on top of the stack to as a string
    * If the value is a primitive, then we use`valueOf` method in `String` class
    * If the value is an object, we invoke `toString` method on the value on top of the stack
    *
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe    Wrapped type of the value on top of the stack
    */
  def javaValueToString(method: MethodVisitor, tpe: WrappedType): Unit = {
    val objectInternalName = asm.Type.getInternalName(Constants.objectClass)
    val stringInternalName = asm.Type.getInternalName(Constants.stringClass)

    tpe match {
      case WrappedPrimitive(Type.Bool) =>
        val boolToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Boolean])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, boolToStringMethod.getName,
          asm.Type.getMethodDescriptor(boolToStringMethod), false)
      case WrappedPrimitive(Type.Char) =>
        val charToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Char])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, charToStringMethod.getName,
          asm.Type.getMethodDescriptor(charToStringMethod), false)
      case WrappedPrimitive(Type.Int8) | WrappedPrimitive(Type.Int16) | WrappedPrimitive(Type.Int32) =>
        val intToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Int])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, intToStringMethod.getName,
          asm.Type.getMethodDescriptor(intToStringMethod), false)
      case WrappedPrimitive(Type.Int64) =>
        val longToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Long])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, longToStringMethod.getName,
          asm.Type.getMethodDescriptor(longToStringMethod), false)
      case WrappedPrimitive(Type.Float32) =>
        val floatToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Float])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, floatToStringMethod.getName,
          asm.Type.getMethodDescriptor(floatToStringMethod), false)
      case WrappedPrimitive(Type.Float64) =>
        val doubleToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Double])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, doubleToStringMethod.getName,
          asm.Type.getMethodDescriptor(doubleToStringMethod), false)
      case _ =>
        val objectToStringMethod = Constants.objectClass.getMethod("toString")
        method.visitMethodInsn(INVOKEVIRTUAL, objectInternalName, objectToStringMethod.getName,
          asm.Type.getMethodDescriptor(objectToStringMethod), false)
    }
  }

  /**
    * This method will get the descriptor of the type of the `field`.
    * If field = `WrappedNonPrimitives(Set(..))` then the field is not a primitive and should be represented by an object, hence we return
    * the descriptor of the object class otherwise the field is a primitive and we return descriptor of that primitive.
    *
    * @param fType Wrapped type of the field
    * @return descriptor of the field
    */
  def getWrappedTypeDescriptor(fType: WrappedType): String = fType match {
    case WrappedPrimitive(tpe) => descriptor(tpe, Map())
    case WrappedNonPrimitives(_) => asm.Type.getDescriptor(Constants.objectClass)
  }

  /**
    * This method box a field with name `name` with type `tpe` on the class `className`
    * If the field is a primitive then it is boxed using the appropriate java type, if it is not a primitive
    * then we just return the field
    *
    * @param method    MethodVisitor used to emit the code to a method
    * @param tpe       Wrapped type of the field to be boxed
    * @param className qualified name of the class that the field is defined on
    * @param name      name of the field to be boxed
    */
  def boxField(method: MethodVisitor, tpe: WrappedType, className: QualName, name: String): Unit = {

    /**
      * This method will box the primitive on top of the stack
      *
      * @param boxedObjectDescriptor descriptor of the boxed version of the primitive
      * @param signature             signature of the constructor of the boxer
      */
    def box(boxedObjectDescriptor: String, signature: String) = {
      method.visitTypeInsn(NEW, boxedObjectDescriptor)
      method.visitInsn(DUP)
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(className), name, getWrappedTypeDescriptor(tpe))
      method.visitMethodInsn(INVOKESPECIAL, boxedObjectDescriptor, "<init>", signature, false)
    }

    // based on the type of the field, we pick the appropriate class that boxes the primitive
    tpe match {
      case WrappedPrimitive(Type.Bool) => box("java/lang/Boolean", "(Z)V")
      case WrappedPrimitive(Type.Char) => box("java/lang/Character", "(C)V")
      case WrappedPrimitive(Type.Int8) => box("java/lang/Byte", "(B)V")
      case WrappedPrimitive(Type.Int16) => box("java/lang/Short", "(S)V")
      case WrappedPrimitive(Type.Int32) => box("java/lang/Integer", "(I)V")
      case WrappedPrimitive(Type.Int64) => box("java/lang/Long", "(J)V")
      case WrappedPrimitive(Type.Float32) => box("java/lang/Float", "(F)V")
      case WrappedPrimitive(Type.Float64) => box("java/lang/Double", "(D)V")
      case _ =>
        method.visitVarInsn(ALOAD, 0)
        method.visitFieldInsn(GETFIELD, decorate(className), name, getWrappedTypeDescriptor(tpe))
    }
  }

  /**
    * If an object is on the top of the stack, then this method will replace it with the hashCode of that object
    * if a primitive is on top of the stack, then the primitive is casted to an int.
    *
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe    Wrapped type of the field
    */
  def getHashCodeOrConvertToInt(method: MethodVisitor, tpe: WrappedType): Unit = {
    val clazz = Constants.objectClass
    val objectMethod = clazz.getMethod("hashCode")

    tpe match {
      case WrappedPrimitive(Type.Bool) | WrappedPrimitive(Type.Char) | WrappedPrimitive(Type.Int8) | WrappedPrimitive(Type.Int16)
           | WrappedPrimitive(Type.Int32) => ()
      case WrappedPrimitive(Type.Int64) => method.visitInsn(L2I)
      case WrappedPrimitive(Type.Float32) => method.visitInsn(F2I)
      case WrappedPrimitive(Type.Float64) => method.visitInsn(D2I)
      case _ => method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), objectMethod.getName,
        asm.Type.getMethodDescriptor(objectMethod), false)
    }
  }

  /**
    * This method returns the appropriate reference class based on the given type
    *
    * @param tpe the given type
    * @return the appropriate reference class for the given type.
    */
  def getReferenceClazz(tpe: Type): Class[_] = tpe match {
    case Type.Apply(Type.Ref, Type.Bool) => Constants.cell$Bool
    case Type.Apply(Type.Ref, Type.Char) => Constants.cell$Char
    case Type.Apply(Type.Ref, Type.Int8) => Constants.cell$Int8
    case Type.Apply(Type.Ref, Type.Int16) => Constants.cell$Int16
    case Type.Apply(Type.Ref, Type.Int32) => Constants.cell$Int32
    case Type.Apply(Type.Ref, Type.Int64) => Constants.cell$Int64
    case Type.Apply(Type.Ref, Type.Float32) => Constants.cell$Float32
    case Type.Apply(Type.Ref, Type.Float64) => Constants.cell$Float64
    case Type.Apply(Type.Ref, _) => Constants.cell$Obj
    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe`")
  }

  /**
    * Returns true if the Type corresponds to a primitive in Java
    */
  def isPrimitive(tpe: Type): Boolean = tpe match {
    case Type.Var(x, k) => false
    case Type.Unit => false
    case Type.Bool => true
    case Type.Char => true
    case Type.Float32 => true
    case Type.Float64 => true
    case Type.Int8 => true
    case Type.Int16 => true
    case Type.Int32 => true
    case Type.Int64 => true
    case Type.BigInt => false
    case Type.Str => false
    case Type.Native => false
    case Type.Ref => false
    case Type.Arrow(l) => false
    case Type.Tuple(l) => false
    case Type.Apply(Type.Tuple(l), ts) => false
    case Type.Apply(Type.Arrow(l), ts) => false
    case Type.Apply(t, ts) => false
    case Type.Enum(enum, kind) => false
  }


  /*
   * Decorate (mangle) a prefix (list of strings) to get the internal JVM name.
   */
  def decorate(qualName: QualName): String = qualName.ref.mkString("/")

  /**
    * Returns the descriptor of the JVM type that `tpe` maps to.
    *
    * The descriptor of a type must be associated with a Context, because the descriptor of a closure object (i.e. a
    * lambda function and not a JVM method) is the descriptor of a generated interface.
    *
    * The inner function may seem weird at first, but we need it because Type.Lambda has two different descriptors
    * depending on how it's used. If we're compiling a method that takes two ints and returns an int,
    * (i.e. its type is Type.Lambda(List(Int, Int), Int)) we need to generate the JVM descriptor `(II)I`.
    * On the other hand, if that same method is being passed as a closure, we need to use the interface that was
    * generated for that closure, and not its JVM type descriptor. We don't want a type descriptor to look like
    * `((II)I)I`.
    */
  def descriptor(tpe: Type, interfaces: Map[Type, FlixClassName]): String = {
    def inner(tpe: Type): String = tpe match {
      case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Unit => asm.Type.getDescriptor(Constants.unitClass)
      case Type.Bool => asm.Type.BOOLEAN_TYPE.getDescriptor
      case Type.Char => asm.Type.CHAR_TYPE.getDescriptor
      case Type.Float32 => asm.Type.FLOAT_TYPE.getDescriptor
      case Type.Float64 => asm.Type.DOUBLE_TYPE.getDescriptor
      case Type.Int8 => asm.Type.BYTE_TYPE.getDescriptor
      case Type.Int16 => asm.Type.SHORT_TYPE.getDescriptor
      case Type.Int32 => asm.Type.INT_TYPE.getDescriptor
      case Type.Int64 => asm.Type.LONG_TYPE.getDescriptor
      case Type.BigInt => asm.Type.getDescriptor(Constants.bigIntegerClass)
      case Type.Str => asm.Type.getDescriptor(Constants.stringClass)
      case Type.Native => asm.Type.getDescriptor(Constants.objectClass)
      case _ if tpe.isArrow => s"L${decorate(interfaces(tpe))};"
      case _ if tpe.isTuple =>
        val targs = tpe.getTypeArguments
        val clazzName = TupleClassName(targs.map(typeToWrappedType))
        s"L${decorate(clazzName)};"
      case _ if tpe.isEnum =>
        val Type.Enum(sym, _) = tpe.getTypeConstructor
        s"L${decorate(EnumInterfName(sym))};"
      case _ if tpe.isRef => asm.Type.getDescriptor(getReferenceClazz(tpe))
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    tpe match {
      case _ if tpe.isArrow =>
        val ts = tpe.getTypeArguments
        s"(${ts.init.map(inner).mkString})${inner(ts.last)}"
      case _ => inner(tpe)
    }
  }

  /**
    * Returns the internal name of the JVM class that `tpe` maps to.
    */
  def internalName(tpe: Type, interfaces: Map[Type, FlixClassName]): String = tpe match {
    case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
    case Type.Unit => asm.Type.getInternalName(Constants.unitClass)
    case Type.BigInt => asm.Type.getInternalName(Constants.bigIntegerClass)
    case Type.Str => asm.Type.getInternalName(Constants.stringClass)
    case Type.Native => asm.Type.getInternalName(Constants.objectClass)
    case _ if tpe.isArrow => decorate(interfaces(tpe))
    case _ if tpe.isTuple =>
      val targs = tpe.getTypeArguments
      val clazzName = TupleClassName(targs.map(typeToWrappedType))
      decorate(clazzName)
    case _ if tpe.isEnum =>
      val Type.Enum(sym, _) = tpe.getTypeConstructor
      decorate(EnumInterfName(sym))
    case _ if tpe.isRef => asm.Type.getInternalName(getReferenceClazz(tpe))
    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }

  /**
    * Constants passed to the asm library to generate bytecode.
    *
    * Originally, we simply used string literals (to encode internal class names, descriptors, etc.) and passed them to
    * the library. This was very brittle for two reasons: 1) changing a class or method would break things and we
    * wouldn't know until run time; and 2) multiple string literals would have to be updated.
    *
    * The solution is to use compile-time reflection whenever possible, and move commonly-used constants into this
    * object. Constants that are used only once are left inline.
    *
    * Compile-time reflection gives us compile-time errors if we change a class and don't update here. However, we are
    * stuck with run-time reflection for certain things, e.g. getting a method. So if we change a method and don't
    * update here, we won't know until we run the tests.
    *
    * Note that we can't easily use reflection to get singleton or package objects. (It is possible with run-time
    * reflection, but it is awkward and we don't get the benefit of compile-time errors.)
    *
    * TODO: Refactor the Value object to be Java-like style so reflection is easier. Or use run-time reflection?
    */
  object Constants {
    val objectClass: Class[_] = classOf[Object]
    val stringClass: Class[_] = classOf[java.lang.String]
    val bigIntegerClass: Class[_] = classOf[java.math.BigInteger]
    val arrayObjectClass: Class[_] = classOf[Array[Object]]
    val setClass: Class[_] = classOf[scala.collection.immutable.Set[Object]]
    val flixClass: Class[_] = classOf[api.Flix]

    val unitClass: Class[_] = classOf[api.Unit]
    val tupleClass: Class[_] = classOf[api.Tuple]
    val cell$Bool: Class[_] = classOf[Cell$Bool]
    val cell$Char: Class[_] = classOf[Cell$Char]
    val cell$Int8: Class[_] = classOf[Cell$Int8]
    val cell$Int16: Class[_] = classOf[Cell$Int16]
    val cell$Int32: Class[_] = classOf[Cell$Int32]
    val cell$Int64: Class[_] = classOf[Cell$Int64]
    val cell$Float32: Class[_] = classOf[Cell$Float32]
    val cell$Float64: Class[_] = classOf[Cell$Float64]
    val cell$Obj: Class[_] = classOf[Cell$Obj]

    val scalaPredef = "scala/Predef$"
    val scalaMathPkg = "scala/math/package$"
    val tagInterface: Class[_] = classOf[api.Enum]
  }

  // This constant is used in LoadBytecode, so we can't put it in the private Constants object.
  val flixObject = "flixObject"

  /**
    * Generates all the names of the functional interfaces used in the Flix program.
    */
  def generateInterfaceNames(consts: List[ExecutableAst.Def])(implicit genSym: GenSym): Map[Type, FlixClassName] = {
    def visit(e: Expression): Set[Type] = e match {
      case Expression.Unit => Set.empty
      case Expression.True => Set.empty
      case Expression.False => Set.empty
      case Expression.Char(lit) => Set.empty
      case Expression.Float32(lit) => Set.empty
      case Expression.Float64(lit) => Set.empty
      case Expression.Int8(lit) => Set.empty
      case Expression.Int16(lit) => Set.empty
      case Expression.Int32(lit) => Set.empty
      case Expression.Int64(lit) => Set.empty
      case Expression.BigInt(lit) => Set.empty
      case Expression.Str(lit) => Set.empty
      case Expression.Var(sym, tpe, loc) => Set.empty
      case Expression.Closure(ref, freeVars, _, tpe, loc) => Set(tpe)
      case Expression.ApplyClo(exp, args, tpe, loc) => visit(exp) ++ args.flatMap(visit)
      case Expression.ApplyDef(name, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyCloTail(exp, args, tpe, loc) => visit(exp) ++ args.flatMap(visit)
      case Expression.ApplyDefTail(name, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => actuals.flatMap(visit).toSet
      case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.Unary(sop, op, exp, tpe, loc) => visit(exp)
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => visit(exp1) ++ visit(exp2) ++ visit(exp3)
      case Expression.Branch(exp, branches, tpe, loc) =>
        visit(exp) ++ (branches flatMap {
          case (sym, br) => visit(br)
        }).toSet
      case Expression.JumpTo(sym, tpe, loc) => Set.empty
      case Expression.Let(sym, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.Is(sym, tag, exp, loc) => visit(exp)
      case Expression.Tag(enum, tag, exp, tpe, loc) => visit(exp)
      case Expression.Untag(sym, tag, exp, tpe, loc) => visit(exp)
      case Expression.Index(base, offset, tpe, loc) => visit(base)
      case Expression.Tuple(elms, tpe, loc) => elms.flatMap(visit).toSet
      case Expression.Ref(exp, tpe, loc) => visit(exp)
      case Expression.Deref(exp, tpe, loc) => visit(exp)
      case Expression.Assign(exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.Existential(params, exp, loc) =>
        ???
      case Expression.Universal(params, exp, loc) =>
        ???
      case Expression.NativeConstructor(constructor, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.NativeField(field, tpe, loc) => Set.empty
      case Expression.NativeMethod(method, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.UserError(tpe, loc) => Set.empty
      case Expression.MatchError(tpe, loc) => Set.empty
      case Expression.SwitchError(tpe, loc) => Set.empty
    }

    val types = consts.flatMap(x => visit(x.exp)).toSet
    types.map { t =>
      val name = Symbol.freshVarSym("FnItf").toString
      val prefix = FlixClassName(List("ca", "uwaterloo", "flix", "runtime", name))
      t -> prefix
    }.toMap
  }


  /**
    * Find any appearance of an enum case in the give expression `e`
    *
    * @param e expression
    * @return List of enum cases in the expression, each element of the list is of form (EnumType, (CaseName, EnumFieldType))
    */
  def findEnumCases(e: Expression): List[(Type, (String, Type))] = e match {
    case Expression.Unit => Nil
    case Expression.True => Nil
    case Expression.False => Nil
    case Expression.Char(lit) => Nil
    case Expression.Float32(lit) => Nil
    case Expression.Float64(lit) => Nil
    case Expression.Int8(lit) => Nil
    case Expression.Int16(lit) => Nil
    case Expression.Int32(lit) => Nil
    case Expression.Int64(lit) => Nil
    case Expression.BigInt(lit) => Nil
    case Expression.Str(lit) => Nil
    case Expression.Var(sym, tpe, loc) => Nil
    case Expression.Closure(ref, freeVars, _, tpe, loc) => Nil
    case Expression.ApplyClo(exp, args, tpe, loc) => findEnumCases(exp) ::: args.flatMap(findEnumCases)
    case Expression.ApplyDef(name, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.ApplyCloTail(exp, args, tpe, loc) => findEnumCases(exp) ::: args.flatMap(findEnumCases)
    case Expression.ApplyDefTail(name, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => actuals.flatMap(findEnumCases)
    case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.Unary(sop, op, exp, tpe, loc) => findEnumCases(exp)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2) ::: findEnumCases(exp3)
    case Expression.Branch(exp, branches, tpe, loc) => findEnumCases(exp) ::: branches.values.flatMap(findEnumCases).toList
    case Expression.JumpTo(sym, tpe, loc) => Nil
    case Expression.Let(sym, exp1, exp2, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2)
    case Expression.Is(sym, tag, exp, loc) => findEnumCases(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => List((tpe, (tag, exp.tpe))) ::: findEnumCases(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc) => List((exp.tpe, (tag, tpe))) ::: findEnumCases(exp)
    case Expression.Index(base, offset, tpe, loc) => findEnumCases(base)
    case Expression.Tuple(elms, tpe, loc) => elms.flatMap(findEnumCases).toList
    case Expression.Ref(exp, tpe, loc) => findEnumCases(exp)
    case Expression.Deref(exp, tpe, loc) => findEnumCases(exp)
    case Expression.Assign(exp1, exp2, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2)
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) => ??? // TODO
    case Expression.Existential(params, exp, loc) => findEnumCases(exp)
    case Expression.Universal(params, exp, loc) => findEnumCases(exp)
    case Expression.NativeConstructor(constructor, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.NativeField(field, tpe, loc) => Nil
    case Expression.NativeMethod(method, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.UserError(tpe, loc) => Nil
    case Expression.MatchError(tpe, loc) => Nil
    case Expression.SwitchError(tpe, loc) => Nil
  }
}
