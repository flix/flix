/*
 * Copyright 2015-2016 Ming-Ho Yee
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
import ca.uwaterloo.flix.language.ast.Ast.Hook
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Definition, Expression, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.{InternalCompilerException, Options}
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter
import org.objectweb.asm.{Type => _, _}

import scala.language.existentials

// TODO: Debugging information

object Codegen {

  /*
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
  private object Constants {
    val objectClass = classOf[Object]
    val stringClass = classOf[java.lang.String]
    val bigIntegerClass = classOf[java.math.BigInteger]
    val arrayObjectClass = classOf[Array[Object]]
    val setClass = classOf[scala.collection.immutable.Set[Object]]
    val flixClass = classOf[Flix]

    val unitClass = Value.Unit.getClass
    val tagClass = classOf[Value.Tag]

    val valueObject = "ca/uwaterloo/flix/runtime/Value$"
    val scalaPredef = "scala/Predef$"
    val scalaMathPkg = "scala/math/package$"

    def loadValueObject(visitor: MethodVisitor): Unit =
      visitor.visitFieldInsn(GETSTATIC, valueObject, "MODULE$", s"L$valueObject;")
  }

  // This constant is used in LoadBytecode, so we can't put it in the private Constants object.
  val flixObject = "flixObject"

  case class Context(prefix: List[String],
                     functions: List[Definition.Constant],
                     declarations: Map[Symbol.DefnSym, Type],
                     interfaces: Map[Type, List[String]]) {

    /*
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
    def descriptor(tpe: Type): String = {
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
        case Type.Apply(Type.FTuple(l), _) => asm.Type.getDescriptor(Constants.arrayObjectClass)
        case _ if tpe.isEnum => asm.Type.getDescriptor(Constants.tagClass)
        case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }

      tpe match {
        case Type.Apply(Type.Arrow(l), ts) => s"(${ts.take(l - 1).map(inner).mkString})${inner(ts.last)}"
        case _ => inner(tpe)
      }
    }
  }

  /*
   * Decorate (mangle) a prefix (list of strings) to get the internal JVM name.
   */
  def decorate(prefix: List[String]): String = prefix.mkString("/")

  /*
   * Compile an interface with a single abstract method `apply` whose signature matches the given type. Furthermore, we
   * annotate the interface with @FunctionalInterface.
   */
  def compileFunctionalInterface(ctx: Context)(tpe: Type): Array[Byte] = {
    val visitor = new ClassWriter(0)
    visitor.visit(V1_8, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, decorate(ctx.prefix), null, asm.Type.getInternalName(Constants.objectClass), null)

    // Add annotation @java.lang.FunctionalInterface
    val av = visitor.visitAnnotation(asm.Type.getDescriptor(classOf[java.lang.FunctionalInterface]), true)
    av.visitEnd()

    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "apply", ctx.descriptor(tpe), null, null)
    mv.visitEnd()

    visitor.visitEnd()
    visitor.toByteArray
  }

  /*
   * Compile a JVM class for the current context. The context contains a list of definitions to compile, as well as the
   * JVM package/class to put the definitions in.
   *
   * Example: The Flix definition A.B.C/foo is compiled as the method foo in class C, within the package A.B.
   */
  def compile(ctx: Context, options: Options): Array[Byte] = {
    // Initialize the class writer.
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)

    // Wrap the class writer in a CheckClassAdapter if compiler invariants are enabled.
    val visitor =
    if (options.invariants)
      new CheckClassAdapter(classWriter)
    else
      classWriter

    // Initialize the visitor to create a class.
    visitor.visit(V1_8, ACC_PUBLIC + ACC_SUPER, decorate(ctx.prefix), null, asm.Type.getInternalName(Constants.objectClass), null)

    compileStaticFlixField(ctx, visitor)
    compileConstructor(ctx, visitor)
    // TODO: Here we filter laws, since the backend does not support existentials/universals, but could we fix that?
    ctx.functions.filterNot(_.ann.isLaw).foreach(compileFunction(ctx, visitor))

    visitor.visitEnd()

    classWriter.toByteArray
  }

  /*
   * Create a static field for the Flix object, and generate the class initializer to initialize the field to null.
   */
  private def compileStaticFlixField(ctx: Context, visitor: ClassVisitor): Unit = {
    val fv = visitor.visitField(ACC_PUBLIC + ACC_STATIC, flixObject, asm.Type.getDescriptor(Constants.flixClass), null, null)
    fv.visitEnd()

    val mv = visitor.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    mv.visitCode()
    mv.visitInsn(ACONST_NULL)
    mv.visitFieldInsn(PUTSTATIC, decorate(ctx.prefix), flixObject, asm.Type.getDescriptor(Constants.flixClass))
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 0)
    mv.visitEnd()
  }

  /*
   * Generate the constructor. Takes a Context and an initialized ClassVisitor.
   */
  private def compileConstructor(ctx: Context, visitor: ClassVisitor): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    val clazz = Constants.objectClass
    val ctor = clazz.getConstructor()
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    // Call the super (java.lang.Object) constructor
    mv.visitMethodInsn(INVOKESPECIAL, asm.Type.getInternalName(clazz), "<init>", asm.Type.getConstructorDescriptor(ctor), false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /*
   * Given a definition for a Flix function, generate bytecode. Takes a Context and an initialized ClassVisitor.
   */
  private def compileFunction(ctx: Context, visitor: ClassVisitor)(function: Definition.Constant): Unit = {
    val flags = if (function.isSynthetic) ACC_PUBLIC + ACC_STATIC + ACC_SYNTHETIC else ACC_PUBLIC + ACC_STATIC
    val mv = visitor.visitMethod(flags, function.sym.suffix, ctx.descriptor(function.tpe), null, null)
    mv.visitCode()

    val entryPoint = new Label()
    mv.visitLabel(entryPoint)

    compileExpression(ctx, mv, entryPoint)(function.exp)

    val tpe = function.tpe match {
      case Type.Apply(Type.Arrow(l), ts) => ts.last
      case _ => throw InternalCompilerException(s"Constant ${function.sym} should have been converted to a function.")
    }

    tpe match {
      case Type.Var(id, kind) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => mv.visitInsn(IRETURN)
      case Type.Int64 => mv.visitInsn(LRETURN)
      case Type.Float32 => mv.visitInsn(FRETURN)
      case Type.Float64 => mv.visitInsn(DRETURN)
      case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.FTuple(_), _) | Type.Apply(Type.Arrow(_), _) |
           Type.Apply(_, _) => mv.visitInsn(ARETURN)
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    // Dummy large numbers (JVM limits) so the bytecode checker can run. Afterwards, the ASM library calculates the proper maxes.
    mv.visitMaxs(65535, 65535)
    mv.visitEnd()
  }

  private def compileExpression(ctx: Context, visitor: MethodVisitor, entryPoint: Label)(expr: Expression): Unit = expr match {
    case Expression.Unit =>
      val clazz = Constants.unitClass
      visitor.visitFieldInsn(GETSTATIC, asm.Type.getInternalName(clazz), "MODULE$", asm.Type.getDescriptor(clazz))
    case Expression.True => visitor.visitInsn(ICONST_1)
    case Expression.False => visitor.visitInsn(ICONST_0)
    case Expression.Char(c) => compileInt(visitor)(c)
    case Expression.Float32(f) => f match {
      case 0f => visitor.visitInsn(FCONST_0)
      case 1f => visitor.visitInsn(FCONST_1)
      case 2f => visitor.visitInsn(FCONST_2)
      case _ => visitor.visitLdcInsn(f)
    }
    case Expression.Float64(d) => d match {
      case 0d => visitor.visitInsn(DCONST_0)
      case 1d => visitor.visitInsn(DCONST_1)
      case _ => visitor.visitLdcInsn(d)
    }
    case Expression.Int8(b) => compileInt(visitor)(b)
    case Expression.Int16(s) => compileInt(visitor)(s)
    case Expression.Int32(i) => compileInt(visitor)(i)
    case Expression.Int64(l) => compileInt(visitor)(l, isLong = true)
    case Expression.BigInt(ii) =>
      // java.math.BigInteger(String) constructor
      val bigint = Constants.bigIntegerClass
      val ctor = bigint.getConstructor(Constants.stringClass)
      val name = asm.Type.getInternalName(bigint)

      visitor.visitTypeInsn(NEW, name)
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(ii.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, name, "<init>", asm.Type.getConstructorDescriptor(ctor), false)
    case Expression.Str(s) => visitor.visitLdcInsn(s)

    case load: LoadExpression => compileLoadExpr(ctx, visitor, entryPoint)(load)
    case store: StoreExpression => compileStoreExpr(ctx, visitor, entryPoint)(store)

    case Expression.Var(sym, tpe, _) => tpe match {
      case Type.Var(id, kind) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ILOAD, sym.getStackOffset)
      case Type.Int64 => visitor.visitVarInsn(LLOAD, sym.getStackOffset)
      case Type.Float32 => visitor.visitVarInsn(FLOAD, sym.getStackOffset)
      case Type.Float64 => visitor.visitVarInsn(DLOAD, sym.getStackOffset)
      case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.Arrow(_), _) |
           Type.Apply(Type.Enum(_, _), _) => visitor.visitVarInsn(ALOAD, sym.getStackOffset)
      case _ if tpe.isTuple => visitor.visitVarInsn(ALOAD, sym.getStackOffset)
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    case Expression.Ref(name, _, _) =>
      // Reference to a top-level definition that isn't used in a MkClosureRef or ApplyRef, so it's a 0-arg function.
      val targetTpe = ctx.declarations(name)
      visitor.visitMethodInsn(INVOKESTATIC, decorate(name.prefix), name.suffix, ctx.descriptor(targetTpe), false)

    case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
      // We create a closure the same way Java 8 does. We use InvokeDynamic and the LambdaMetafactory. The idea is that
      // LambdaMetafactory creates a CallSite (linkage), and then the CallSite target is invoked (capture) to create a
      // function object. Later, at ApplyRef, the function object is called (invocation).
      //
      // For more information, see:
      // http://cr.openjdk.java.net/~briangoetz/lambda/lambda-translation.html
      // http://docs.oracle.com/javase/8/docs/api/java/lang/invoke/LambdaMetafactory.html

      // Load the capture values. We push them onto the stack, since they are the dynamic arguments of the InvokeDynamic
      // instruction (i.e. the arguments to the lambda object constructor).
      // We construct Expression.Var nodes and compile them as expected.
      for (f <- freeVars) {
        val v = Expression.Var(f.sym, f.tpe, loc)
        compileExpression(ctx, visitor, entryPoint)(v)
      }

      // The name of the method implemented by the lambda.
      val invokedName = "apply"

      // The type descriptor of the CallSite. Its arguments are the types of capture variables, and its return
      // type is the interface the lambda object implements (i.e. the type of the closure).
      val csTpe = Type.mkArrow(freeVars.toList.map(_.tpe), tpe)
      val invokedType = ctx.descriptor(csTpe)

      // The handle for the bootstrap method we pass to InvokeDynamic, which is
      // `java.lang.invoke.LambdaMetafactory.metafactory(...)`.
      val clazz = classOf[java.lang.invoke.LambdaMetafactory]
      val method = clazz.getMethods.filter(m => m.getName == "metafactory").head
      val bsmHandle = new Handle(H_INVOKESTATIC, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method))

      // The arguments array for the bootstrap method. Note that the JVM automatically provides the first three
      // arguments (caller, invokedName, invokedType). We need to explicitly provide the remaining three arguments:
      //
      //   samMethodType - Signature and return type of method implemented by the lambda object. Note that this is a
      //                   type, not a string. We convert the Flix type to a JVM method descriptor to an ASM type.
      //
      //   implMethod - Method handle describing the implementation method which should be called at invocation time.
      //
      //   instantiatedMethodType - Signature and return type that should be enforced dynamically at invocation time.
      //                            Can be a specialization of `samMethodType` due to type erasure, but it's the same in
      //                            our implementation because we don't use generics for lambdas (we compile specialized
      //                            versions of functional interfaces).
      //
      // Note that samMethodType and instantiatedMethodType take ASM types, and represent the type of the function
      // object, while implMethod takes a descriptor string and represents the implementation method's type (that is,
      // with the capture variables included in the arguments list).
      val samMethodType = asm.Type.getType(ctx.descriptor(tpe))
      val implMethod = new Handle(H_INVOKESTATIC, decorate(ref.sym.prefix), ref.sym.suffix, ctx.descriptor(ref.tpe))
      val instantiatedMethodType = asm.Type.getType(ctx.descriptor(tpe))
      val bsmArgs = Array(samMethodType, implMethod, instantiatedMethodType)

      // Finally, generate the InvokeDynamic instruction.
      visitor.visitInvokeDynamicInsn(invokedName, invokedType, bsmHandle, bsmArgs: _*)

    case Expression.ApplyRef(name, args, _, _) =>
      // We know what function we're calling, so we can look up its signature.
      val targetTpe = ctx.declarations(name)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the call.
      args.foreach(compileExpression(ctx, visitor, entryPoint))
      visitor.visitMethodInsn(INVOKESTATIC, decorate(name.prefix), name.suffix, ctx.descriptor(targetTpe), false)

    case Expression.ApplyTail(name, formals, actuals, _, _) =>
      // Evaluate each argument and push the result on the stack.
      // Compute the stack height taken by the arguments based on their types.
      var globalOffset: Int = 0
      for (arg <- actuals) {
        // Evaluate the argument and push the result on the stack.
        compileExpression(ctx, visitor, entryPoint)(arg)

        // Update stack height used by the argument.
        arg.tpe match {
          case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => globalOffset += 1
          case Type.Int64 => globalOffset += 2
          case Type.Float32 => globalOffset += 1
          case Type.Float64 => globalOffset += 2
          case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(_, _) => globalOffset += 1
          case tpe => throw InternalCompilerException(s"Unexpected type '$tpe'.")
        }
      }

      // Overwrite each local variable with the value on the stack.
      // The values are on the stack in reverse order, so we must iterate over the arguments in reverse order.
      var offset = globalOffset
      for (arg <- actuals.reverse) {
        // Overwrite the local variable with the value on the stack.
        arg.tpe match {
          case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 =>
            offset -= 1
            visitor.visitVarInsn(ISTORE, offset)
          case Type.Int64 =>
            offset -= 2
            visitor.visitVarInsn(LSTORE, offset)
          case Type.Float32 =>
            offset -= 1
            visitor.visitVarInsn(FSTORE, offset)
          case Type.Float64 =>
            offset -= 2
            visitor.visitVarInsn(DSTORE, offset)
          case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(_, _) =>
            offset -= 1
            visitor.visitVarInsn(ASTORE, offset)
          case _ => throw InternalCompilerException(s"Not yet implemented.") // TODO
        }
      }

      // Jump to the entry point of the method.
      visitor.visitJumpInsn(GOTO, entryPoint)

    case Expression.ApplyHook(hook, args, tpe, _) =>
      val (isSafe, name, elmsClass) = hook match {
        case _: Hook.Safe => (true, "invoke", classOf[IValue])
        case _: Hook.Unsafe => (false, "invokeUnsafe", Constants.objectClass)
      }
      val clazz = Constants.flixClass
      val method = clazz.getMethods.filter(m => m.getName == name).head

      // First we get the Flix object from the static field.
      visitor.visitFieldInsn(GETSTATIC, decorate(ctx.prefix), flixObject, asm.Type.getDescriptor(clazz))

      // Next we load the arguments for the invoke/invokeUnsafe virtual call, starting with the name of the hook.
      visitor.visitLdcInsn(hook.sym.toString)

      // Create the arguments array.
      compileInt(visitor)(args.length)
      visitor.visitTypeInsn(ANEWARRAY, asm.Type.getInternalName(elmsClass))

      // Evaluate the arguments left-to-right, putting them into the array.
      for ((e, i) <- args.zipWithIndex) {
        // Duplicate the array reference, otherwise AASTORE will consume it.
        visitor.visitInsn(DUP)
        compileInt(visitor)(i)

        // Load the actual element. If we're calling a safe hook, we need to wrap the value.
        if (isSafe) {
          val clazz2 = classOf[WrappedValue]
          val ctor = clazz2.getConstructors.head
          visitor.visitTypeInsn(NEW, asm.Type.getInternalName(clazz2))
          visitor.visitInsn(DUP)
          compileBoxedExpr(ctx, visitor, entryPoint)(e)
          visitor.visitMethodInsn(INVOKESPECIAL, asm.Type.getInternalName(clazz2), "<init>", asm.Type.getConstructorDescriptor(ctor), false)
        } else {
          compileBoxedExpr(ctx, visitor, entryPoint)(e)
        }
        visitor.visitInsn(AASTORE)
      }

      // Finally, make the virtual call to invoke/invokeUnsafe. If it's a safe hook, we also need to call `getUnsafeRef`.
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
      if (isSafe) {
        val method2 = elmsClass.getMethod("getUnsafeRef")
        visitor.visitMethodInsn(INVOKEINTERFACE, asm.Type.getInternalName(elmsClass), method2.getName, asm.Type.getMethodDescriptor(method2), true)
      }

      // Unbox the result, if necessary.
      compileUnbox(ctx, visitor)(tpe)

    case Expression.ApplyClosure(exp, args, _, _) =>
      // Lambdas are called through an interface. We don't know what function we're calling, but we know its type,
      // so we can lookup the interface we're calling through.
      val name = ctx.interfaces(exp.tpe)

      // Evaluate the function we're calling.
      compileExpression(ctx, visitor, entryPoint)(exp)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the interface call.
      args.foreach(compileExpression(ctx, visitor, entryPoint))
      visitor.visitMethodInsn(INVOKEINTERFACE, decorate(name), "apply", ctx.descriptor(exp.tpe), true)

    case Expression.Unary(op, exp, _, _) => compileUnaryExpr(ctx, visitor, entryPoint)(op, exp)
    case Expression.Binary(op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => compileArithmeticExpr(ctx, visitor, entryPoint)(o, exp1, exp2)
      case o: ComparisonOperator => compileComparisonExpr(ctx, visitor, entryPoint)(o, exp1, exp2)
      case o: LogicalOperator => compileLogicalExpr(ctx, visitor, entryPoint)(o, exp1, exp2)
      case o: BitwiseOperator => compileBitwiseExpr(ctx, visitor, entryPoint)(o, exp1, exp2)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(ctx, visitor, entryPoint)(exp1)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(ctx, visitor, entryPoint)(exp2)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(ctx, visitor, entryPoint)(exp3)
      visitor.visitLabel(ifEnd)

    case Expression.Let(sym, exp1, exp2, _, _) =>
      compileExpression(ctx, visitor, entryPoint)(exp1)
      exp1.tpe match {
        case Type.Var(id, kind) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id.")
        case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ISTORE, sym.getStackOffset)
        case Type.Int64 => visitor.visitVarInsn(LSTORE, sym.getStackOffset)
        case Type.Float32 => visitor.visitVarInsn(FSTORE, sym.getStackOffset)
        case Type.Float64 => visitor.visitVarInsn(DSTORE, sym.getStackOffset)
        case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.FTuple(_), _) | Type.Apply(Type.Arrow(_), _) => visitor.visitVarInsn(ASTORE, sym.getStackOffset)
        case Type.Apply(_, _) => visitor.visitVarInsn(ASTORE, sym.getStackOffset)
        case tpe => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }
      compileExpression(ctx, visitor, entryPoint)(exp2)

    case Expression.Is(exp, tag, _) =>
      // Value.Tag.tag() method
      val clazz1 = Constants.tagClass
      val method1 = clazz1.getMethod("tag")

      // java.lang.String.equals(Object) method
      val clazz2 = Constants.stringClass
      val method2 = clazz2.getMethod("equals", Constants.objectClass)

      // Get the tag string of `exp` (compiled as a tag) and compare to `tag.name`.
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz1), method1.getName, asm.Type.getMethodDescriptor(method1), false)
      visitor.visitLdcInsn(tag)
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz2), method2.getName, asm.Type.getMethodDescriptor(method2), false)

    case Expression.Tag(enum, tag, exp, _, _) =>
      // Load the Value singleton object, then the arguments (tag.name, boxing if necessary), and finally call `Value.mkTag`.
      Constants.loadValueObject(visitor)
      visitor.visitLdcInsn(tag)
      compileBoxedExpr(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkTag", "(Ljava/lang/String;Ljava/lang/Object;)Lca/uwaterloo/flix/runtime/Value$Tag;", false)

    case Expression.Untag(tag, exp, tpe, _) =>
      // Value.Tag.value() method
      val clazz = Constants.tagClass
      val method = clazz.getMethod("value")

      // Compile `exp` as a tag expression, get its inner `value`, and unbox if necessary.
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
      compileUnbox(ctx, visitor)(tpe)

    case Expression.Index(base, offset, tpe, _) =>
      // Emit code for the base (tuple) expression.
      compileExpression(ctx, visitor, entryPoint)(base)

      // A tuple is represented as an array of objects.
      visitor.visitTypeInsn(CHECKCAST, asm.Type.getDescriptor(Constants.arrayObjectClass))

      // Emit code for the array index.
      compileInt(visitor)(offset)

      // Load the value at the index on the stack.
      visitor.visitInsn(AALOAD)

      // Unbox the value, if necessary.
      compileUnbox(ctx, visitor)(tpe)

    case Expression.Tuple(elms, _, _) =>
      // Push the length of the tuple on the stack.
      compileInt(visitor)(elms.length)

      // Allocate an array of objects to store the elements of the tuple.
      visitor.visitTypeInsn(ANEWARRAY, asm.Type.getInternalName(Constants.objectClass))

      // Emit code for each component of the tuple and store it into the array.
      for ((e, i) <- elms.zipWithIndex) {
        // Duplicate the array reference, since AASTORE consumes it.
        visitor.visitInsn(DUP)

        // Push the array index.
        compileInt(visitor)(i)

        // Emit code for the component, box if necessary.
        compileBoxedExpr(ctx, visitor, entryPoint)(e)

        // Store the value into the array.
        visitor.visitInsn(AASTORE)
      }

    case Expression.Existential(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$expr' at ${loc.source.format}.")

    case Expression.Universal(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$expr' at ${loc.source.format}.")

    case Expression.NativeConstructor(constructor, args, tpe, loc) => ??? // TODO

    case Expression.NativeField(field, tpe, loc) => ??? // TODO

    case Expression.NativeMethod(method, args, tpe, loc) => ??? // TODO

    case Expression.UserError(_, loc) =>
      val name = asm.Type.getInternalName(classOf[UserException])
      val msg = s"User exception: ${loc.format}."
      compileException(visitor, name, msg)

    case Expression.MatchError(_, loc) =>
      val name = asm.Type.getInternalName(classOf[MatchException])
      val msg = s"Non-exhaustive match expression: ${loc.format}."
      compileException(visitor, name, msg)

    case Expression.SwitchError(_, loc) =>
      val name = asm.Type.getInternalName(classOf[SwitchException])
      val msg = s"Non-exhaustive switch expression: ${loc.format}."
      compileException(visitor, name, msg)
  }

  private def compileException(visitor: MethodVisitor, name: String, msg: String): Unit = {
    visitor.visitTypeInsn(NEW, name)
    visitor.visitInsn(DUP)
    visitor.visitLdcInsn(msg)
    // TODO: Load actual source location or change the exception
    visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "MODULE$", "Lca/uwaterloo/flix/language/ast/package$SourceLocation$;")
    visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "Unknown", "()Lca/uwaterloo/flix/language/ast/package$SourceLocation;", false)
    visitor.visitMethodInsn(INVOKESPECIAL, name, "<init>", "(Ljava/lang/String;Lca/uwaterloo/flix/language/ast/package$SourceLocation;)V", false)
    visitor.visitInsn(ATHROW)
  }

  /*
   * Some types (e.g. bool, int, str) need to be boxed as Flix values.
   * Other types (e.g. tag, tuple) are already Flix values.
   */
  private def compileBoxedExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)(exp: Expression): Unit = exp.tpe match {
    case Type.Bool =>
      // If we know the value of the boolean expression, then compile it directly rather than calling mkBool.
      Constants.loadValueObject(visitor)
      exp match {
        case Expression.True => visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "True", "()Ljava/lang/Object;", false)
        case Expression.False => visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "False", "()Ljava/lang/Object;", false)
        case _ =>
          compileExpression(ctx, visitor, entryPoint)(exp)
          visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkBool", "(Z)Ljava/lang/Object;", false)
      }

    case Type.Char =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkChar", "(C)Ljava/lang/Object;", false)

    case Type.Float32 =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkFloat32", "(F)Ljava/lang/Object;", false)

    case Type.Float64 =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkFloat64", "(D)Ljava/lang/Object;", false)

    case Type.Int8 =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkInt8", "(I)Ljava/lang/Object;", false)

    case Type.Int16 =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkInt16", "(I)Ljava/lang/Object;", false)

    case Type.Int32 =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkInt32", "(I)Ljava/lang/Object;", false)

    case Type.Int64 =>
      Constants.loadValueObject(visitor)
      compileExpression(ctx, visitor, entryPoint)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "mkInt64", "(J)Ljava/lang/Object;", false)

    case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.FTuple(_), _) | Type.Apply(Type.Arrow(_), _) |
         Type.Apply(Type.Enum(_, _), _) => compileExpression(ctx, visitor, entryPoint)(exp)

    case tpe => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }

  /*
   * The value at the top of the stack is boxed as a Flix Value, and needs to be unboxed (e.g. to an int, boolean, or
   * String), or it needs to be cast to a specific Value type (e.g. Value.Unit.type, Value.Tag).
   *
   * Note that the generated code here is slightly more efficient than calling `cast2XX` since we don't have to branch.
   */
  private def compileUnbox(ctx: Context, visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
    case Type.Unit => visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(Constants.unitClass))

    case Type.Bool | Type.Char | Type.Float32 | Type.Float64 | Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 =>
      val (methodName, clazz): (String, Class[_]) = tpe match {
        case Type.Bool => ("booleanValue", classOf[java.lang.Boolean])
        case Type.Char => ("charValue", classOf[java.lang.Character])
        case Type.Float32 => ("floatValue", classOf[java.lang.Float])
        case Type.Float64 => ("doubleValue", classOf[java.lang.Double])
        case Type.Int8 => ("byteValue", classOf[java.lang.Byte])
        case Type.Int16 => ("shortValue", classOf[java.lang.Short])
        case Type.Int32 => ("intValue", classOf[java.lang.Integer])
        case Type.Int64 => ("longValue", classOf[java.lang.Long])
        case _ => throw new InternalCompilerException(s"Type $tpe should not be handled in this case.")
      }
      val method = clazz.getMethod(methodName)
      val name = asm.Type.getInternalName(clazz)
      val desc = asm.Type.getMethodDescriptor(method)
      visitor.visitTypeInsn(CHECKCAST, name)
      visitor.visitMethodInsn(INVOKEVIRTUAL, name, methodName, desc, false)

    case Type.BigInt | Type.Str | Type.Enum(_, _) | Type.Apply(Type.Arrow(_), _) | Type.Apply(Type.Enum(_, _), _) =>
      val name = tpe match {
        case Type.BigInt => asm.Type.getInternalName(Constants.bigIntegerClass)
        case Type.Str => asm.Type.getInternalName(Constants.stringClass)
        case Type.Enum(_, _) | Type.Apply(Type.Enum(_, _), _) => asm.Type.getInternalName(Constants.tagClass)
        case Type.Apply(Type.Arrow(l), _) =>
          // TODO: Is this correct? Need to write a test when we can write lambda expressions.
          decorate(ctx.interfaces(tpe))
        case _ => throw new InternalCompilerException(s"Type $tpe should not be handled in this case.")
      }
      visitor.visitTypeInsn(CHECKCAST, name)

    case Type.Native => // Don't need to cast AnyRef to anything

    case Type.Apply(Type.FTuple(l), _) =>
      visitor.visitTypeInsn(CHECKCAST, asm.Type.getDescriptor(Constants.arrayObjectClass))

    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }

  /*
   * (e >> offset).toInt & mask
   *
   * Example:
   * x represents a bit with unknown value (0 or 1)
   *   load   = LoadInt8(e, 16)
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 10101010 xxxxxxxx xxxxxxxx
   *
   * First we do a right shift (with sign extension) (LSHR):
   *            xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 10101010
   * Then we convert/truncate to an int, discarding the higher-order bits (L2I):
   *            xxxxxxxx xxxxxxxx xxxxxxxx 10101010
   * We bitwise-and with the mask, clearing the higher-order bits (IAND):
   *   mask   = 00000000 00000000 00000000 11111111
   *   result = 00000000 00000000 00000000 10101010
   *
   * If we used I2B instead of a mask, sign extension would give:
   *            11111111 11111111 11111111 10101010
   */
  private def compileLoadExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)(load: LoadExpression): Unit = {
    compileExpression(ctx, visitor, entryPoint)(load.e)
    if (load.offset > 0) {
      compileInt(visitor)(load.offset)
      visitor.visitInsn(LSHR)
    }
    visitor.visitInsn(L2I)
    compileInt(visitor)(load.mask)
    visitor.visitInsn(IAND)
  }

  /*
   * (e & targetMask) | ((v.toLong & mask) << offset)
   *
   * Example:
   * x represents a bit with unknown value (0 or 1)
   *   store  = StoreInt32(e, 0, v)
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
   *   v      = 11110000 11110000 11110000 11110000
   *
   * First we bitwise-and with targetMask to clear target/destination bits (LAND):
   *   tMask  = 11111111 11111111 11111111 11111111 00000000 00000000 00000000 00000000
   *   result = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 00000000 00000000 00000000 00000000
   * Then we convert v to a long (with sign extension) (I2L):
   *            11111111 11111111 11111111 11111111 11110000 11110000 11110000 11110000
   * Then we bitwise-and with the mask, clearing the higher-order bits (LAND):
   *   mask   = 00000000 00000000 00000000 00000000 11111111 11111111 11111111 11111111
   *   result = 00000000 00000000 00000000 00000000 11110000 11110000 11110000 11110000
   * In this example, we don't left shift because the shift offset is 0 (LSHL). We bitwise-or with e to get the final
   * result (LOR):
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 00000000 00000000 00000000 00000000
   *   result = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 11110000 11110000 11110000 11110000
   *
   * Note: (v & mask).toLong instead of (v.toLong & mask) gives the wrong result because of sign extension.
   * Bitwise-and of v and mask (IAND):
   *   mask   = 11111111 11111111 11111111 11111111
   *   result = 11110000 11110000 11110000 11110000
   * Convert int to long (doing a sign extension) (I2L):
   *   result = 11111111 11111111 11111111 11111111 11110000 11110000 11110000 11110000
   * Again in this example, we don't do a left shift. But when we do a bitwise-or, we overwrite the bits of e (LOR):
   *   e      = xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx 00000000 00000000 00000000 00000000
   *   result = 11111111 11111111 11111111 11111111 11110000 11110000 11110000 11110000
   */
  private def compileStoreExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)(store: StoreExpression): Unit = {
    compileExpression(ctx, visitor, entryPoint)(store.e)
    compileInt(visitor)(store.targetMask, isLong = true)
    visitor.visitInsn(LAND)
    compileExpression(ctx, visitor, entryPoint)(store.v)
    visitor.visitInsn(I2L)
    compileInt(visitor)(store.mask, isLong = true)
    visitor.visitInsn(LAND)
    if (store.offset > 0) {
      compileInt(visitor)(store.offset)
      visitor.visitInsn(LSHL)
    }
    visitor.visitInsn(LOR)
  }

  /*
   * Generate code to load an integer constant.
   *
   * Uses the smallest number of bytes necessary, e.g. ICONST_0 takes 1 byte to load a 0, but BIPUSH 7 takes 2 bytes to
   * load a 7, and SIPUSH 200 takes 3 bytes to load a 200. However, note that values on the stack normally take up 4
   * bytes. The exception is if we set `isLong` to true, in which case a cast will be performed if necessary.
   *
   * This is needed because sometimes we expect the operands to be a long, which means two (int) values are popped from
   * the stack and concatenated to form a long.
   */
  private def compileInt(visitor: MethodVisitor)(i: Long, isLong: Boolean = false): Unit = {
    i match {
      case -1 => visitor.visitInsn(ICONST_M1)
      case 0 => if (!isLong) visitor.visitInsn(ICONST_0) else visitor.visitInsn(LCONST_0)
      case 1 => if (!isLong) visitor.visitInsn(ICONST_1) else visitor.visitInsn(LCONST_1)
      case 2 => visitor.visitInsn(ICONST_2)
      case 3 => visitor.visitInsn(ICONST_3)
      case 4 => visitor.visitInsn(ICONST_4)
      case 5 => visitor.visitInsn(ICONST_5)
      case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => visitor.visitIntInsn(BIPUSH, i.toInt)
      case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => visitor.visitIntInsn(SIPUSH, i.toInt)
      case _ if scala.Int.MinValue <= i && i <= scala.Int.MaxValue => visitor.visitLdcInsn(i.toInt)
      case _ => visitor.visitLdcInsn(i)
    }
    if (isLong && scala.Int.MinValue <= i && i <= scala.Int.MaxValue && i != 0 && i != 1) visitor.visitInsn(I2L)
  }

  private def compileUnaryExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)(op: UnaryOperator, e: Expression): Unit = {
    compileExpression(ctx, visitor, entryPoint)(e)
    op match {
      case UnaryOperator.LogicalNot =>
        val condElse = new Label()
        val condEnd = new Label()
        visitor.visitJumpInsn(IFNE, condElse)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)
      case UnaryOperator.Plus => // nop
      case UnaryOperator.Minus => compileUnaryMinusExpr(ctx, visitor)(e.tpe)
      case UnaryOperator.BitwiseNegate => compileUnaryNegateExpr(ctx, visitor)(e.tpe)
    }
  }

  /*
   * For Int8/Int16, we need to truncate and sign extend the result.
   *
   * Example:
   * Suppose we store the value -128 into an Int8 (byte). The number is represented as (in two's complement):
   *   10000000
   * But on the JVM, the value is sign extended and stored as an Int32 (int):
   *   11111111 11111111 11111111 10000000
   * If we simply negate -128, we get the value 128, which is represented as:
   *   00000000 00000000 00000000 10000000
   * But this is greater than the maximum value (127) for an Int8 (byte). We use I2B to convert the Int32 (int) to an
   * Int8 (byte), which does a truncation and sign extension:
   *   11111111 11111111 11111111 10000000
   * And the final value is -128.
   *
   * Note that in Java semantics, the unary minus operator returns an Int32 (int), so the programmer must explicitly
   * cast to an Int8 (byte).
   */
  private def compileUnaryMinusExpr(ctx: Context, visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
    case Type.Float32 => visitor.visitInsn(FNEG)
    case Type.Float64 => visitor.visitInsn(DNEG)
    case Type.Int8 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2B)
    case Type.Int16 =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2S)
    case Type.Int32 => visitor.visitInsn(INEG)
    case Type.Int64 => visitor.visitInsn(LNEG)
    case Type.BigInt =>
      // java.math.BigInteger.negate() method
      val clazz = Constants.bigIntegerClass
      val method = clazz.getMethod("negate")
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false);
    case _ => throw InternalCompilerException(s"Can't apply UnaryOperator.Minus to type $tpe.")
  }

  /*
   * Note that ~xxxx = xxxx ^ 1111, and since the JVM uses two's complement, -1 = 0xFFFFFFFF, so ~b = b ^ -1. No need to
   * truncate because Int8/Int16 (byte/short) are sign extended to Int32 (int), and s.ext(negate(b) = negate(s.ext(b)).
   *
   * Example:
   * Consider two Int8s:
   *     b = 11000011    c = 00001111
   * Conceptually, ~b and ~c would be:
   *    ~b = 00111100   ~c = 11110000
   * On the JVM, b, ~b, c, and ~c would be stored as an Int32s:
   *    b' = 11111111 11111111 11111111 11000011    c' = 00000000 00000000 00000000 00001111
   *   ~b' = 00000000 00000000 00000000 00111100   ~c' = 11111111 11111111 11111111 11110000
   *
   * Note that sign extending and then negating a value is equal to negating and then sign extending it.
   */
  private def compileUnaryNegateExpr(ctx: Context, visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
    case Type.Int8 | Type.Int16 | Type.Int32 =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(IXOR)
    case Type.Int64 =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(I2L)
      visitor.visitInsn(LXOR)
    case Type.BigInt =>
      // java.math.BigInteger.not() method
      val clazz = Constants.bigIntegerClass
      val method = clazz.getMethod("not")
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false);
    case _ => throw InternalCompilerException(s"Can't apply UnaryOperator.Negate to type $tpe.")
  }

  /*
   * Results are truncated (and sign extended), so that adding two IntN's will always return an IntN. Overflow can
   * occur. Note that in Java semantics, the result of an arithmetic operation is an Int32 (int) or an Int64 (long), and
   * the user must explicitly downcast to an Int8 (byte) or Int16 (short).
   *
   * Example:
   * Consider adding two Int8s (bytes), 127 and 1. The result overflows:
   *     01111111 =  127
   *   + 00000001 =    1
   * --------------------
   *     10000000 = -128
   * However, on the JVM, Int8s (bytes) are represented as Int32s (ints). The result of an arithmetic operation is an
   * Int32 (int), and there is no overflow (in this case):
   *     00000000 00000000 00000000 01111111 =  127
   *   + 00000000 00000000 00000000 00000001 =    1
   * -----------------------------------------------
   *     00000000 00000000 00000000 10000000 =  128
   * We want the value to be an Int8 (byte), so we use I2B to truncate and sign extend:
   *     11111111 11111111 11111111 10000000 = -128
   *
   * Exponentiation takes a separate codepath. Values must be cast to doubles (F2D, I2D, L2D; note that bytes and shorts
   * are represented as ints and so we use I2D), then we invoke the static method `math.pow`, and then we have to cast
   * back to the original type (D2F, D2I, D2L; note that bytes and shorts need to be cast again with I2B and I2S).
   */
  private def compileArithmeticExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)
                                   (o: ArithmeticOperator, e1: Expression, e2: Expression): Unit = {
    if (o == BinaryOperator.Exponentiate) {
      val (castToDouble, castFromDouble) = e1.tpe match {
        case Type.Float32 => (F2D, D2F)
        case Type.Float64 => (NOP, NOP) // already a double
        case Type.Int8 | Type.Int16 | Type.Int32 => (I2D, D2I)
        case Type.Int64 => (L2D, D2L)
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
      }
      visitor.visitFieldInsn(GETSTATIC, Constants.scalaMathPkg, "MODULE$", s"L${Constants.scalaMathPkg};")
      compileExpression(ctx, visitor, entryPoint)(e1)
      visitor.visitInsn(castToDouble)
      compileExpression(ctx, visitor, entryPoint)(e2)
      visitor.visitInsn(castToDouble)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.scalaMathPkg, "pow", "(DD)D", false)
      visitor.visitInsn(castFromDouble)
      (e1.tpe: @unchecked) match {
        case Type.Int8 => visitor.visitInsn(I2B)
        case Type.Int16 => visitor.visitInsn(I2S)
        case Type.Float32 | Type.Float64 | Type.Int32 | Type.Int64 => visitor.visitInsn(NOP)
      }
    } else {
      compileExpression(ctx, visitor, entryPoint)(e1)
      compileExpression(ctx, visitor, entryPoint)(e2)
      val (intOp, longOp, floatOp, doubleOp, bigIntOp) = o match {
        case BinaryOperator.Plus => (IADD, LADD, FADD, DADD, "add")
        case BinaryOperator.Minus => (ISUB, LSUB, FSUB, DSUB, "subtract")
        case BinaryOperator.Times => (IMUL, LMUL, FMUL, DMUL, "multiply")
        case BinaryOperator.Divide => (IDIV, LDIV, FDIV, DDIV, "divide")
        case BinaryOperator.Modulo => (IREM, LREM, FREM, DREM, "remainder")
        case BinaryOperator.Exponentiate => throw InternalCompilerException("BinaryOperator.Exponentiate already handled.")
      }
      e1.tpe match {
        case Type.Float32 => visitor.visitInsn(floatOp)
        case Type.Float64 => visitor.visitInsn(doubleOp)
        case Type.Int8 =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2B)
        case Type.Int16 =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2S)
        case Type.Int32 => visitor.visitInsn(intOp)
        case Type.Int64 => visitor.visitInsn(longOp)
        case Type.BigInt =>
          // java.math.BigInteger.{bigIntOp}() method
          val clazz = Constants.bigIntegerClass
          val method = clazz.getMethod(bigIntOp, clazz)
          visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), bigIntOp, asm.Type.getMethodDescriptor(method), false);
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe} near ${e1.loc.format}")
      }
    }
  }

  /*
   * Ints, Floats, and Chars support all six comparison operations (LE, LT, GE, GT, EQ, NE), but Unit, Bools, Strings,
   * Enums, Tuples, and Sets only support EQ and NE. Note that the generated code uses the negated condition, i.e.
   * branch if the (source) condition is false.
   *
   * Some reference types (Unit and String) can use reference equality because of interning.
   *
   * Int8/16/32 and Char comparisons only need a single instruction (IF_ICMPyy, where yy is one of
   * {LE, LT, GE, GT, EQ, NE}), which jumps if the yy condition is true, i.e. the (source) condition is false. All other
   * types do a comparison first (LCMP, {F,D}CMP{G,L}), and then a branch (IFyy).
   *
   * Specifically, LCMP can be represented in pseudocode as:
   *
   *     if (v1 > v2)        1
   *     else if (v1 == v2)  0
   *     else if (v1 < v2)  -1
   *
   * Then the result is used in the IFyy comparison to determine which branch to take. So the pair of instructions
   * for comparing longs (LCMP, IFyy) is similar to the single instruction for comparing ints (IF_ICMPyy).
   *
   * Float32/64 is similar, using xCMPz instead of LCMP, where x is one of {F,D} and z is one of {G,L}. z is necessary
   * to handle the fact that a float can be NaN (which is unordered), and any comparison involving NaN must fail.
   * xCMPG and xCMPL are the same, except for how they handle NaN. If either operand is NaN, xCMPG will push 1 onto the
   * stack, while xCMPL will push -1. In pseudocode:
   *
   *     if (v1 > v2)        1
   *     else if (v1 == v2)  0
   *     else if (v1 < v2)  -1
   *     else if (v1 is NaN || v2 is NaN)
   *       if (xCMPG)       1
   *       else if (xCMPL) -1
   *
   * For more information, see the following:
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-3.html#jvms-3.5
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.if_icmp_cond
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.lcmp
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.fcmp_op
   * http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.if_cond
   *
   * BigInts are compared using the `compareTo` method.
   * `bigint1 OP bigint2` is compiled as `bigint1.compareTo(bigint2) OP 0`.
   */
  private def compileComparisonExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)
                                   (o: ComparisonOperator, e1: Expression, e2: Expression): Unit = {
    e1.tpe match {
      case Type.Enum(_, _) | Type.Apply(Type.FTuple(_), _) | Type.Apply(Type.Enum(_, _), _) if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
        (e1.tpe: @unchecked) match {
          case Type.Apply(Type.FTuple(_), _) | Type.Enum(_, _) | Type.Apply(Type.Enum(_, _), _) =>
            // Emit code to call Value.equal(Object, Object).
            Constants.loadValueObject(visitor)
            compileExpression(ctx, visitor, entryPoint)(e1)
            compileExpression(ctx, visitor, entryPoint)(e2)
            visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.valueObject, "equal", "(Ljava/lang/Object;Ljava/lang/Object;)Z", false)
        }
        if (o == BinaryOperator.NotEqual) {
          val condElse = new Label()
          val condEnd = new Label()
          visitor.visitJumpInsn(IFEQ, condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_1)
          visitor.visitLabel(condEnd)
        }
      case _ =>
        compileExpression(ctx, visitor, entryPoint)(e1)
        compileExpression(ctx, visitor, entryPoint)(e2)
        val condElse = new Label()
        val condEnd = new Label()
        val (intOp, floatOp, doubleOp, refOp, cmp) = o match {
          case BinaryOperator.Less => (IF_ICMPGE, FCMPG, DCMPG, NOP, IFGE)
          case BinaryOperator.LessEqual => (IF_ICMPGT, FCMPG, DCMPG, NOP, IFGT)
          case BinaryOperator.Greater => (IF_ICMPLE, FCMPL, DCMPL, NOP, IFLE)
          case BinaryOperator.GreaterEqual => (IF_ICMPLT, FCMPL, DCMPL, NOP, IFLT)
          case BinaryOperator.Equal => (IF_ICMPNE, FCMPG, DCMPG, IF_ACMPNE, IFNE)
          case BinaryOperator.NotEqual => (IF_ICMPEQ, FCMPG, DCMPG, IF_ACMPEQ, IFEQ)
        }
        e1.tpe match {
          case Type.Unit | Type.Str if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // Unit and String can be reference compared for equality.
            visitor.visitJumpInsn(refOp, condElse)
          case Type.Bool if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // Bool can be (value) compared for equality.
            visitor.visitJumpInsn(intOp, condElse)
          case Type.Float32 =>
            visitor.visitInsn(floatOp)
            visitor.visitJumpInsn(cmp, condElse)
          case Type.Float64 =>
            visitor.visitInsn(doubleOp)
            visitor.visitJumpInsn(cmp, condElse)
          case Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitJumpInsn(intOp, condElse)
          case Type.Int64 =>
            visitor.visitInsn(LCMP)
            visitor.visitJumpInsn(cmp, condElse)
          case Type.BigInt =>
            // java.math.BigInteger.compareTo(java.math.BigInteger)
            val clazz = Constants.bigIntegerClass
            val method = clazz.getMethod("compareTo", clazz)
            visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(intOp, condElse)
          case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe} near ${e1.loc.format}")
        }
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, condEnd)
        visitor.visitLabel(condElse)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(condEnd)
    }
  }

  /*
   * Note that LogicalAnd, LogicalOr, and Implication do short-circuit evaluation.
   * Implication and Biconditional are rewritten to their logical equivalents, and then compiled.
   */
  private def compileLogicalExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)
                                (o: LogicalOperator, e1: Expression, e2: Expression): Unit = o match {
    case BinaryOperator.LogicalAnd =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(ctx, visitor, entryPoint)(e1)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(ctx, visitor, entryPoint)(e2)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      visitor.visitInsn(ICONST_1)
      visitor.visitJumpInsn(GOTO, andEnd)
      visitor.visitLabel(andFalseBranch)
      visitor.visitInsn(ICONST_0)
      visitor.visitLabel(andEnd)
    case BinaryOperator.LogicalOr =>
      val orTrueBranch = new Label()
      val orFalseBranch = new Label()
      val orEnd = new Label()
      compileExpression(ctx, visitor, entryPoint)(e1)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(ctx, visitor, entryPoint)(e2)
      visitor.visitJumpInsn(IFEQ, orFalseBranch)
      visitor.visitLabel(orTrueBranch)
      visitor.visitInsn(ICONST_1)
      visitor.visitJumpInsn(GOTO, orEnd)
      visitor.visitLabel(orFalseBranch)
      visitor.visitInsn(ICONST_0)
      visitor.visitLabel(orEnd)
  }

  /*
   * In general we don't do any truncation, because it doesn't matter what the higher-order bits are.
   *
   * Example:
   * Consider the bitwise-and of the following Int8s:
   *     11110000             00000011
   *   & 11000000           & 11001111
   * -------------        -------------
   *     11000000             00000011
   * On the JVM, these Int8s (bytes) would be represented as Int32s (ints):
   *    11111111 11111111 11111111 11110000        00000000 00000000 00000000 00000011
   *  & 11111111 11111111 11111111 11000000      & 00000000 00000000 00000000 11001111
   * ---------------------------------------    ---------------------------------------
   *    11111111 11111111 11111111 11000000        00000000 00000000 00000000 00000011
   *
   * As with Unary.Negate, sign extension before or after the operation yields the same result.
   *
   *
   * The exception is with bitwise left shifts. The higher-order bits matter because we might sign extend.
   *
   * Example:
   * Consider the following left shift, where x and y each represent unknown values (0 or 1):
   *   x000y000 << 4 = y0000000
   * But because Int8s (bytes) are represented as Int32s (ints), and the x is sign extended, we get:
   *   xxxxxxxx xxxxxxxx xxxxxxxx x000y000 << 4 = xxxxxxxx xxxxxxxx xxxxx000 y0000000
   * We truncate and sign extend (I2B), which gives:
   *   yyyyyyyy yyyyyyyy yyyyyyyy y0000000
   *
   * It doesn't matter that we left shifted x, because we (generally) ignore the higher-order bits. However, it *does*
   * matter that we shifted y into the sign bit of an Int8. If y = 1, then the Int8 (byte) 10000000 has value -128,
   * which needs to be sign extended to represent that value as an Int32 (int).
   *
   * Example:
   * Consider the following (signed) right shift, where x represents an unknown value (0 or 1):
   *   x0000000 >> 4 = xxxxx000
   * These Int8s (bytes) are represented as Int32s (ints), so the x is sign extended:
   *   xxxxxxxx xxxxxxxx xxxxxxxx x0000000 >> 4 = xxxxxxxx xxxxxxxx xxxxxxxx xxxxx000
   *
   * We don't need to truncate, because it is impossible for random data to be in the higher-order bits. Either those
   * bits are all 0, or they are 1 (because of sign extension).
   *
   * Note: the right-hand operand of a shift (i.e. the shift amount) *must* be Int32.
   */
  private def compileBitwiseExpr(ctx: Context, visitor: MethodVisitor, entryPoint: Label)
                                (o: BitwiseOperator, e1: Expression, e2: Expression): Unit = {
    compileExpression(ctx, visitor, entryPoint)(e1)
    compileExpression(ctx, visitor, entryPoint)(e2)
    val (intOp, longOp, bigintOp) = o match {
      case BinaryOperator.BitwiseAnd => (IAND, LAND, "and")
      case BinaryOperator.BitwiseOr => (IOR, LOR, "or")
      case BinaryOperator.BitwiseXor => (IXOR, LXOR, "xor")
      case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL, "shiftLeft")
      case BinaryOperator.BitwiseRightShift => (ISHR, LSHR, "shiftRight")
    }
    e1.tpe match {
      case Type.Int8 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2B)
      case Type.Int16 =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2S)
      case Type.Int32 => visitor.visitInsn(intOp)
      case Type.Int64 => visitor.visitInsn(longOp)
      case Type.BigInt =>
        val clazz = Constants.bigIntegerClass
        val method = clazz.getMethods.filter(m => m.getName == bigintOp).head
        visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), bigintOp, asm.Type.getMethodDescriptor(method), false);
      case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
    }
  }

}
