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

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Definition, Expression}
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.runtime.Value
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
    * Wrapper around qualified name of a class
    */
  sealed trait QualName{
    /**
      * A list representing the qualified name of a class
      */
    def ref: List[String]
  }

  /**
    * Simple implementation of `QualName` used to generate classes containing flix functions and functional interfaces
    */
  case class FlixClassName(ref: List[String]) extends QualName

  /**
    * Qualified name of an enum class
    * @param sym symbol of the enum
    * @param tag tag of the enum
    * @param tpe tpe of the field of the enum
    */
  case class EnumClassName(sym: EnumSym, tag: String, tpe: Option[Type]) extends QualName {
    val ref: List[String] = tpe match {
      case Some(t) if isPrimitive(t) => CodegenHelper.fixedEnumPrefix ::: sym.namespace ::: List(sym.name, t.toString, tag)
      case _ => CodegenHelper.fixedEnumPrefix ::: sym.namespace ::: List(sym.name, "object", tag)
    }
  }

  /**
    * Qualified name of an enum interface
    * @param sym symbol of the enum
    */
  case class EnumInterfName(sym: EnumSym) extends QualName {
    val ref: List[String] = CodegenHelper.fixedEnumPrefix ::: sym.namespace ::: List(sym.name, "EnumInterface")
  }

  /**
    * Qualified name of a tuple class
    * @param fields fields of the tuple
    */
  case class TupleClassName(fields: List[Option[Type]]) extends QualName {
    val ref: List[String] = fixedTuplePrefix ::: fields.map{
      case Some(t : Type) => t.toString
      case _ => "object"
    } ::: List("Tuple")
  }

  /**
    * This method returns a string which uniquely specifies the give type. If the type can be represented by a primitive,
    * then the name of that premitive is returned, otherwise if the type has to be represented by an object then string
    * `object` is return
    * @param tpe type to be specified
    */
  def typeSpecifier(tpe: Type) : String = {
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
      case Type.Ref => objectStr
      case Type.Arrow(l) => objectStr
      case Type.FTuple(l) => objectStr
      case Type.Apply(Type.FTuple(l), ts) =>objectStr
      case Type.Apply(Type.Arrow(l), ts) => objectStr
      case Type.Apply(t, ts) => objectStr
      case Type.Enum(enum, kind) => objectStr
    }
  }

  /**
    * If a type is primitive, we wrap it around `Some`, else if the type requires an object to be represented, we replace
    * the type with `None`
    * @param tpe the type to be transformed
    */
  def transformTypeToOptionType(tpe: Type): Option[Type] = tpe match {
    case t if isPrimitive(t) => Some(t)
    case _ => None
  }

  /**
    * Generates a field for the class with with name `name`, with descriptor `descriptor`
    * using `visitor`
    * For example calling this method with name = `field01` and descriptor = `I` creates the following field:
    *
    * public int field01;
    *
    * calling this method with name = `value` and descriptor = `java.lang.Object` creates the following:
    *
    * public Object value;
    *
    * @param visitor class visitor
    * @param name name of the field
    * @param descriptor descriptor of field
    */
  def compileField(visitor: ClassWriter, name: String, descriptor: String) : Unit = {
    val field = visitor.visitField(ACC_PUBLIC, name, descriptor, null, null)
    field.visitEnd()
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    * @param tpe type of the value to be loaded, if it is `None` the value to be loaded is an Object
    * @return Appropriate load instruction for the given type
    */
  def getLoadInstruction(tpe: Option[Type]) : Int = tpe match {
    case Some(Type.Var(id, _)) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '${tpe.get}'.")
    case Some(Type.Bool) => ILOAD
    case Some(Type.Char) => ILOAD
    case Some(Type.Int8) => ILOAD
    case Some(Type.Int16) => ILOAD
    case Some(Type.Int32) => ILOAD
    case Some(Type.Int64) => LLOAD
    case Some(Type.Float32) => FLOAD
    case Some(Type.Float64) => DLOAD
    case _ => ALOAD
  }

  /**
    * This function is called to compare two values on top of the stack with the type `tpe`.
    * We will pick the appropriate comparison between the two values, if they are not equal, we
    * jump to the `label`, otherwise we continue with the current control flow.
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe type of the values to be compared, if it is `None` values to be compared are of the type Object
    * @param label label in case that values on top of the stack are not equal
    */
  def branchIfNotEqual(method: MethodVisitor, tpe: Option[Type], label: Label) : Unit = {
    val clazz = Constants.objectClass
    val objectEqualsMethod = clazz.getMethod("equals", clazz)

    tpe match {
      case Some(Type.Var(id, kind)) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '${tpe.get}'.")
      case Some(Type.Bool) | Some(Type.Char) | Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) =>
        method.visitJumpInsn(IF_ICMPNE, label)
      case Some(Type.Int64) =>
        method.visitInsn(LCMP)
        method.visitJumpInsn(IFNE, label)
      case Some(Type.Float32) =>
        method.visitInsn(FCMPG)
        method.visitJumpInsn(IFNE, label)
      case Some(Type.Float64) =>
        method.visitInsn(DCMPG)
        method.visitJumpInsn(IFNE, label)
      case _ =>
        method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), objectEqualsMethod.getName,
          asm.Type.getMethodDescriptor(objectEqualsMethod), false)
        method.visitJumpInsn(IFEQ, label)
    }
  }

  /**
    * This method is used to represent the value on top of the stack to as a string
    * If the value is a primitive, then we use`valueOf` method in `String` class
    * If the value is an object, we invoke `toString` method on the value on top of the stack
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe type of the value on top of the stack
    */
  def javaValueToString(method: MethodVisitor, tpe: Option[Type]): Unit = {
    val objectInternalName = asm.Type.getInternalName(Constants.objectClass)
    val stringInternalName = asm.Type.getInternalName(Constants.stringClass)

    tpe match {
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
  }

  /**
    * This method will get the descriptor of the type of the `field`.
    * If field = `None` then the field is not a primitive and should be represented by an object, hence we return
    * the descriptor of the object class
    * @param fType type of the field
    * @return descriptor of the field
    */
  def getFieldOptionDescriptor(fType: Option[Type]) : String = fType match {
    case Some(tpe : Type) => descriptor(tpe, Map())
    case _ => asm.Type.getDescriptor(Constants.objectClass)
  }

  /**
    * This method box a field with name `name` with type `tpe` on the class `className`
    * If the field is a primitive then it is boxed using the appropriate java type, if it is not a primitive
    * then we just return the field
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe type of the field to be boxed
    * @param className qualified name of the class that the field is defined on
    * @param name name of the field to be boxed
    */
  def boxField(method: MethodVisitor, tpe: Option[Type], className: QualName, name: String) : Unit = {

    /**
      * This method will box the primitive on top of the stack
      * @param boxedObjectDescriptor descriptor of the boxed version of the primitive
      * @param signature signature of the constructor of the boxer
      */
    def box(boxedObjectDescriptor: String, signature: String) = {
      method.visitTypeInsn(NEW, boxedObjectDescriptor)
      method.visitInsn(DUP)
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(className), name, getFieldOptionDescriptor(tpe))
      method.visitMethodInsn(INVOKESPECIAL, boxedObjectDescriptor, "<init>", signature, false)
    }

    // based on the type of the field, we pick the appropriate class that boxes the primitive
    tpe match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '${tpe.get}'.")
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
        method.visitFieldInsn(GETFIELD, decorate(className), name, getFieldOptionDescriptor(tpe))
    }
  }

  /**
    * If an object is on the top of the stack, then this method will replace it with the hashCode of that object
    * if a primitive is on top of the stack, then the primitive is casted to an int.
    * @param method MethodVisitor used to emit the code to a method
    * @param tpe type of the field
    */
  def getHashCodeOrConvertToInt(method: MethodVisitor, tpe: Option[Type]) : Unit = {
    val clazz = Constants.objectClass
    val objectMethod = clazz.getMethod("hashCode")

    tpe match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '${tpe.get}'.")
      case Some(Type.Bool) | Some(Type.Char) | Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) =>
      case Some(Type.Int64) => method.visitInsn(L2I)
      case Some(Type.Float32) => method.visitInsn(F2I)
      case Some(Type.Float64) => method.visitInsn(D2I)
      case _ => method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), objectMethod.getName,
        asm.Type.getMethodDescriptor(objectMethod), false)
    }
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
    case Type.FTuple(l) => false
    case Type.Apply(Type.FTuple(l), ts) => false
    case Type.Apply(Type.Arrow(l), ts) => false
    case Type.Apply(t, ts) => false
    case Type.Enum(enum, kind) => false
  }


  /*
   * Decorate (mangle) a prefix (list of strings) to get the internal JVM name.
   */
  def decorate(qualName: QualName): String = qualName.ref.mkString("/")

  /**
    * Returns the internal name of the JVM type that `tpe` maps to.
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
      case Type.Apply(Type.Arrow(l), _) => s"L${decorate(interfaces(tpe))};"
      case Type.Apply(Type.FTuple(l), lst) =>
        val clazzName = TupleClassName(lst.map(transformTypeToOptionType))
        s"L${decorate(clazzName)};"
      case _ if tpe.isEnum =>
        val sym = tpe match {
          case Type.Apply(Type.Enum(s, _), _) => s
          case Type.Enum(s, _) => s
          case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
        }
        s"L${decorate(EnumInterfName(sym))};"
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    tpe match {
      case Type.Apply(Type.Arrow(l), ts) => s"(${ts.take(l - 1).map(inner).mkString})${inner(ts.last)}"
      case _ => inner(tpe)
    }
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
    val objectClass : Class[_] = classOf[Object]
    val stringClass : Class[_] = classOf[java.lang.String]
    val bigIntegerClass : Class[_]= classOf[java.math.BigInteger]
    val arrayObjectClass : Class[_] = classOf[Array[Object]]
    val setClass : Class[_] = classOf[scala.collection.immutable.Set[Object]]
    val flixClass : Class[_] = classOf[Flix]

    val unitClass : Class[_] = Value.Unit.getClass
    val tupleClass : Class[_] = classOf[TupleInterface]
    val scalaPredef = "scala/Predef$"
    val scalaMathPkg = "scala/math/package$"
    val tagInterface : Class[_] = classOf[TagInterface]
  }

  // This constant is used in LoadBytecode, so we can't put it in the private Constants object.
  val flixObject = "flixObject"

  /**
    * Generates all the names of the functional interfaces used in the Flix program.
    */
  def generateInterfaceNames(consts: List[Definition.Constant])(implicit genSym: GenSym): Map[Type, FlixClassName] = {
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
      case Expression.LoadBool(n, o) => Set.empty
      case Expression.LoadInt8(b, o) => Set.empty
      case Expression.LoadInt16(b, o) => Set.empty
      case Expression.LoadInt32(b, o) => Set.empty
      case Expression.StoreBool(b, o, v) => Set.empty
      case Expression.StoreInt8(b, o, v) => Set.empty
      case Expression.StoreInt16(b, o, v) => Set.empty
      case Expression.StoreInt32(b, o, v) => Set.empty
      case Expression.Var(sym, tpe, loc) => Set.empty
      case Expression.Ref(name, tpe, loc) => Set.empty
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => Set(tpe)
      case Expression.ApplyRef(name, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyTail(name, formals, actuals, tpe, loc) => actuals.flatMap(visit).toSet
      case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyClosure(exp, args, tpe, loc) => visit(exp) ++ args.flatMap(visit)
      case Expression.Unary(op, exp, tpe, loc) => visit(exp)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => visit(exp1) ++ visit(exp2) ++ visit(exp3)
      case Expression.Let(sym, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.Is(sym, tag, exp, loc) => visit(exp)
      case Expression.Tag(enum, tag, exp, tpe, loc) => visit(exp)
      case Expression.Untag(sym, tag, exp, tpe, loc) => visit(exp)
      case Expression.Index(base, offset, tpe, loc) => visit(base)
      case Expression.Tuple(elms, tpe, loc) => elms.flatMap(visit).toSet
      case Expression.Reference(exp, tpe, loc) => ??? // TODO
      case Expression.Dereference(exp, tpe, loc) => ??? // TODO
      case Expression.Assignment(exp1, exp2, tpe, loc) => ??? // TODO
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
    case Expression.LoadBool(n, o) => Nil
    case Expression.LoadInt8(b, o) => Nil
    case Expression.LoadInt16(b, o) => Nil
    case Expression.LoadInt32(b, o) => Nil
    case Expression.StoreBool(b, o, v) => Nil
    case Expression.StoreInt8(b, o, v) => Nil
    case Expression.StoreInt16(b, o, v) => Nil
    case Expression.StoreInt32(b, o, v) => Nil
    case Expression.Var(sym, tpe, loc) => Nil
    case Expression.Ref(name, tpe, loc) => Nil
    case Expression.MkClosureRef(ref, freeVars, tpe, loc) => Nil
    case Expression.ApplyRef(name, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.ApplyTail(name, formals, actuals, tpe, loc) => actuals.flatMap(findEnumCases)
    case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(findEnumCases)
    case Expression.ApplyClosure(exp, args, tpe, loc) => findEnumCases(exp) ::: args.flatMap(findEnumCases)
    case Expression.Unary(op, exp, tpe, loc) => findEnumCases(exp)
    case Expression.Binary(op, exp1, exp2, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2) ::: findEnumCases(exp3)
    case Expression.Let(sym, exp1, exp2, tpe, loc) => findEnumCases(exp1) ::: findEnumCases(exp2)
    case Expression.Is(sym, tag, exp, loc) => findEnumCases(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => List((tpe, (tag, exp.tpe))) ::: findEnumCases(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc)  => List((exp.tpe, (tag, tpe))) ::: findEnumCases(exp)
    case Expression.Index(base, offset, tpe, loc) => findEnumCases(base)
    case Expression.Tuple(elms, tpe, loc) => elms.flatMap(findEnumCases).toList
    case Expression.Reference(exp, tpe, loc) => ??? // TODO
    case Expression.Dereference(exp, tpe, loc) => ??? // TODO
    case Expression.Assignment(exp1, exp2, tpe, loc) => ??? // TODO
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
