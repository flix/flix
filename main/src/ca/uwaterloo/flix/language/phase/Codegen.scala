package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.ExecutableAst.{Definition, Expression, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter
import org.objectweb.asm.{Type => _, _}

// TODO: For now, we hardcode the type descriptors for all the Value objects
// There's no nice way of using reflection to get the type of a companion object.
// Later, we'll rewrite Value in a Java-like style so reflection is easier
// Or at the very least, move the (common) names into static fields
// TODO: Actually, it looks like "Value.Unit.getClass" will work

// TODO: Debugging information

object Codegen {

  case class Context(prefix: List[String],
                     functions: List[Definition.Constant],
                     declarations: Map[Symbol.Resolved, Type],
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
        case Type.Unit => asm.Type.getDescriptor(Value.Unit.getClass)
        case Type.Bool => asm.Type.BOOLEAN_TYPE.getDescriptor
        case Type.Char => asm.Type.CHAR_TYPE.getDescriptor
        case Type.Float32 => asm.Type.FLOAT_TYPE.getDescriptor
        case Type.Float64 => asm.Type.DOUBLE_TYPE.getDescriptor
        case Type.Int8 => asm.Type.BYTE_TYPE.getDescriptor
        case Type.Int16 => asm.Type.SHORT_TYPE.getDescriptor
        case Type.Int32 => asm.Type.INT_TYPE.getDescriptor
        case Type.Int64 => asm.Type.LONG_TYPE.getDescriptor
        case Type.BigInt => asm.Type.getDescriptor(classOf[java.math.BigInteger])
        case Type.Str => asm.Type.getDescriptor(classOf[java.lang.String])
        case Type.Enum(_, _) => asm.Type.getDescriptor(classOf[Value.Tag])
        case Type.Tuple(_) => asm.Type.getDescriptor(classOf[Value.Tuple])
        case Type.FSet(_) => asm.Type.getDescriptor(classOf[scala.collection.immutable.Set[AnyRef]])
        case Type.Lambda(_, _) => s"L${decorate(interfaces(tpe))};"
        case Type.Tag(_, _, _) => throw InternalCompilerException(s"No corresponding JVM type for $tpe.")
      }

      tpe match {
        case Type.Lambda(args, retTpe) => s"(${args.map(inner).mkString})${inner(retTpe)}"
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
    visitor.visit(V1_8, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, decorate(ctx.prefix), null, "java/lang/Object", null)

    val av = visitor.visitAnnotation("Ljava/lang/FunctionalInterface;", true)
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
  def compile(ctx: Context): Array[Byte] = {
    val functions = ctx.functions
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val visitor = new CheckClassAdapter(classWriter)

    // Initialize the visitor to create a class.
    visitor.visit(V1_8, ACC_PUBLIC + ACC_SUPER, decorate(ctx.prefix), null, "java/lang/Object", null)

    compileConstructor(ctx, visitor)
    functions.foreach(compileFunction(ctx, visitor))

    visitor.visitEnd()
    classWriter.toByteArray
  }

  /*
   * Generate the constructor. Takes a Context and an initialized ClassVisitor.
   */
  private def compileConstructor(ctx: Context, visitor: ClassVisitor): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /*
   * Given a definition for a Flix function, generate bytecode. Takes a Context and an initialized ClassVisitor.
   */
  private def compileFunction(ctx: Context, visitor: ClassVisitor)(function: Definition.Constant): Unit = {
    val flags = if (function.isSynthetic) ACC_PUBLIC + ACC_STATIC + ACC_SYNTHETIC else ACC_PUBLIC + ACC_STATIC
    val mv = visitor.visitMethod(flags, function.name.suffix, ctx.descriptor(function.tpe), null, null)
    mv.visitCode()

    compileExpression(ctx, mv)(function.exp)

    val tpe = function.tpe match {
      case t: Type.Lambda => t.retTpe
      case _ => function.tpe
    }

    tpe match {
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => mv.visitInsn(IRETURN)
      case Type.Int64 => mv.visitInsn(LRETURN)
      case Type.Float32 => mv.visitInsn(FRETURN)
      case Type.Float64 => mv.visitInsn(DRETURN)
      case Type.Unit | Type.BigInt | Type.Str | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) | Type.FSet(_) =>
        mv.visitInsn(ARETURN)
      case Type.Tag(_, _, _) => throw InternalCompilerException(s"Functions can't return type $tpe.")
    }

    // Dummy large numbers so the bytecode checker can run. Afterwards, the ASM library calculates the proper maxes.
    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  private def compileExpression(ctx: Context, visitor: MethodVisitor)(expr: Expression): Unit = expr match {
    case Expression.Unit =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$Unit$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$Unit$;")
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
      visitor.visitTypeInsn(NEW, "java/math/BigInteger")
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(ii.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false)
    case Expression.Str(s) => visitor.visitLdcInsn(s)

    case load: LoadExpression => compileLoadExpr(ctx, visitor)(load)
    case store: StoreExpression => compileStoreExpr(ctx, visitor)(store)

    case Expression.Var(ident, offset, tpe, _) => tpe match {
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 =>
        visitor.visitVarInsn(ILOAD, offset)
      case Type.Int64 => visitor.visitVarInsn(LLOAD, offset)
      case Type.Float32 => visitor.visitVarInsn(FLOAD, offset)
      case Type.Float64 => visitor.visitVarInsn(DLOAD, offset)
      case Type.Unit | Type.BigInt | Type.Str | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) | Type.FSet(_) =>
        visitor.visitVarInsn(ALOAD, offset)
      case Type.Tag(_, _, _) => throw InternalCompilerException(s"Can't have a value of type $tpe.")
    }

    case Expression.Ref(name, _, _) =>
      // Reference to a top-level definition that isn't used in a MkClosureRef or ApplyRef, so it's a 0-arg function.
      val targetTpe = ctx.declarations(name)
      visitor.visitMethodInsn(INVOKESTATIC, decorate(name.prefix), name.suffix, ctx.descriptor(targetTpe), false)

    case Expression.Hook(hook, _, _) => ???

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
        val v = Expression.Var(f.ident, f.offset, f.tpe, loc)
        compileExpression(ctx, visitor)(v)
      }

      // The name of the method implemented by the lambda.
      val invokedName = "apply"

      // The type descriptor of the CallSite. Its arguments are the types of capture variables, and its return
      // type is the interface the lambda object implements (i.e. the type of the closure).
      val csTpe = Type.Lambda(freeVars.toList.map(_.tpe), tpe)
      val invokedType = ctx.descriptor(csTpe)

      // The handle for the bootstrap method we pass to InvokeDynamic, which is
      // `java.lang.invoke.LambdaMetafactory.metafactory`.
      val bsmHandle = new Handle(
        H_INVOKESTATIC,
        "java/lang/invoke/LambdaMetafactory",
        "metafactory",
        "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
      )

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
      val bsmArgs = Array(
        asm.Type.getType(ctx.descriptor(tpe)),
        new Handle(H_INVOKESTATIC, decorate(ref.name.prefix), ref.name.suffix, ctx.descriptor(ref.tpe)),
        asm.Type.getType(ctx.descriptor(tpe))
      )

      // Finally, generate the InvokeDynamic instruction.
      visitor.visitInvokeDynamicInsn(invokedName, invokedType, bsmHandle, bsmArgs: _*)

    case Expression.ApplyRef(name, args, _, _) =>
      // We know what function we're calling, so we can look up its signature.
      val targetTpe = ctx.declarations(name)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the call.
      args.foreach(compileExpression(ctx, visitor))
      visitor.visitMethodInsn(INVOKESTATIC, decorate(name.prefix), name.suffix, ctx.descriptor(targetTpe), false)

    case Expression.ApplyClosure(exp, args, _, _) =>
      // Lambdas are called through an interface. We don't know what function we're calling, but we know its type,
      // so we can lookup the interface we're calling through.
      val name = ctx.interfaces(exp.tpe)

      // Evaluate the function we're calling.
      compileExpression(ctx, visitor)(exp)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the interface call.
      args.foreach(compileExpression(ctx, visitor))
      visitor.visitMethodInsn(INVOKEINTERFACE, decorate(name), "apply", ctx.descriptor(exp.tpe), true)

    case Expression.Unary(op, exp, _, _) => compileUnaryExpr(ctx, visitor)(op, exp)
    case Expression.Binary(op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => compileArithmeticExpr(ctx, visitor)(o, exp1, exp2)
      case o: ComparisonOperator => compileComparisonExpr(ctx, visitor)(o, exp1, exp2)
      case o: LogicalOperator => compileLogicalExpr(ctx, visitor)(o, exp1, exp2)
      case o: BitwiseOperator => compileBitwiseExpr(ctx, visitor)(o, exp1, exp2)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(ctx, visitor)(exp1)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(ctx, visitor)(exp2)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(ctx, visitor)(exp3)
      visitor.visitLabel(ifEnd)

    case Expression.Let(ident, offset, exp1, exp2, _, _) =>
      compileExpression(ctx, visitor)(exp1)
      exp1.tpe match {
        case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 =>
          visitor.visitVarInsn(ISTORE, offset)
        case Type.Int64 => visitor.visitVarInsn(LSTORE, offset)
        case Type.Float32 => visitor.visitVarInsn(FSTORE, offset)
        case Type.Float64 => visitor.visitVarInsn(DSTORE, offset)
        case Type.Unit | Type.BigInt | Type.Str | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) | Type.FSet(_) =>
          visitor.visitVarInsn(ASTORE, offset)
        case Type.Tag(_, _, _) => throw InternalCompilerException(s"Can't have a value of type ${exp1.tpe}.")
      }
      compileExpression(ctx, visitor)(exp2)

    case Expression.CheckTag(tag, exp, _) =>
      // Get the tag string of `exp` (compiled as a tag) and compare to `tag.name`.
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tag", "tag", "()Ljava/lang/String;", false)
      visitor.visitLdcInsn(tag.name)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false)

    case Expression.GetTagValue(tag, exp, tpe, _) =>
      // Compile `exp` as a tag expression, get its inner `value`, and unbox if necessary.
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tag", "value", "()Ljava/lang/Object;", false)
      compileUnbox(ctx, visitor)(tpe)

    case Expression.Tag(enum, tag, exp, _, _) =>
      // Load the Value companion object, then the arguments, and finally call `Value.mkTag`.
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")

      // Load `enum` as a string, by calling `Symbol.Resolved.mk`
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/Symbol$Resolved$", "MODULE$", "Lca/uwaterloo/flix/language/ast/Symbol$Resolved$;")
      visitor.visitLdcInsn(enum.fqn)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/Symbol$Resolved$", "mk", "(Ljava/lang/String;)Lca/uwaterloo/flix/language/ast/Symbol$Resolved;", false)

      // Load `tag.name` and box `exp` if necessary.
      visitor.visitLdcInsn(tag.name)
      compileBoxedExpr(ctx, visitor)(exp)

      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkTag", "(Lca/uwaterloo/flix/language/ast/Symbol$Resolved;Ljava/lang/String;Ljava/lang/Object;)Lca/uwaterloo/flix/runtime/Value$Tag;", false)

    case Expression.GetTupleIndex(base, offset, tpe, _) =>
      // Compile the tuple expression, load the tuple array, compile the offset, load the array element, and unbox if
      // necessary.
      compileExpression(ctx, visitor)(base)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tuple", "elms", "()[Ljava/lang/Object;", false)
      compileInt(visitor)(offset)
      visitor.visitInsn(AALOAD)
      compileUnbox(ctx, visitor)(tpe)

    case Expression.Tuple(elms, _, _) =>
      // Create the array to hold the tuple elements.
      compileInt(visitor)(elms.length)
      visitor.visitTypeInsn(ANEWARRAY, asm.Type.getInternalName(classOf[AnyRef]))

      // Iterate over elms, boxing them and slotting each one into the array.
      for ((e, i) <- elms.zipWithIndex) {
        // Duplicate the array reference, otherwise AASTORE will consume it.
        visitor.visitInsn(DUP)
        compileInt(visitor)(i)
        compileBoxedExpr(ctx, visitor)(e)
        visitor.visitInsn(AASTORE)
      }

      // Now construct a Value.Tuple: create a reference, load the arguments, and call the constructor.
      visitor.visitTypeInsn(NEW, "ca/uwaterloo/flix/runtime/Value$Tuple")

      // We use dup_x1 and swap to manipulate the stack so we can avoid using a local variable.
      // Stack before: array, tuple (top)
      // Stack after: tuple, tuple, array (top)
      visitor.visitInsn(DUP_X1)
      visitor.visitInsn(SWAP)

      // Finally, call the constructor, which pops the reference (tuple) and argument (array).
      visitor.visitMethodInsn(INVOKESPECIAL, "ca/uwaterloo/flix/runtime/Value$Tuple", "<init>", "([Ljava/lang/Object;)V", false)

    case Expression.CheckNil(exp, _) => ???
    case Expression.CheckCons(exp, _) => ???

    case Expression.FSet(elms, _, _) =>
      // First create a scala.immutable.Set
      visitor.visitFieldInsn(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "scala/Predef$", "Set", "()Lscala/collection/immutable/Set$;", false)
      visitor.visitFieldInsn(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;")

      // Create an array to hold the set elements
      compileInt(visitor)(elms.length)
      visitor.visitTypeInsn(ANEWARRAY, asm.Type.getInternalName(classOf[AnyRef]))

      // Iterate over elms, boxing them and slotting each one into the array.
      for ((e, i) <- elms.zipWithIndex) {
        // Duplicate the array reference, otherwise AASTORE will consume it.
        visitor.visitInsn(DUP)
        compileInt(visitor)(i)
        compileBoxedExpr(ctx, visitor)(e)
        visitor.visitInsn(AASTORE)
      }

      // Wrap the array and construct the set
      visitor.visitMethodInsn(INVOKEVIRTUAL, "scala/Predef$", "wrapRefArray", "([Ljava/lang/Object;)Lscala/collection/mutable/WrappedArray;", false)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "scala/collection/immutable/Set$", "apply", "(Lscala/collection/Seq;)Lscala/collection/GenTraversable;", false)

    case Expression.UserError(_, loc) =>
      visitor.visitTypeInsn(NEW, "ca/uwaterloo/flix/api/UserException")
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(s"User exception: ${loc.format}.")
      // TODO: Load actual source location or change UserException
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "MODULE$", "Lca/uwaterloo/flix/language/ast/package$SourceLocation$;")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "Unknown", "()Lca/uwaterloo/flix/language/ast/package$SourceLocation;", false)
      visitor.visitMethodInsn(INVOKESPECIAL, "ca/uwaterloo/flix/api/UserException", "<init>", "(Ljava/lang/String;Lca/uwaterloo/flix/language/ast/package$SourceLocation;)V", false)
      visitor.visitInsn(ATHROW)

    case Expression.MatchError(_, loc) =>
      visitor.visitTypeInsn(NEW, "ca/uwaterloo/flix/api/MatchException")
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(s"Non-exhaustive match expression: ${loc.format}.")
      // TODO: Load actual source location or change MatchException
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "MODULE$", "Lca/uwaterloo/flix/language/ast/package$SourceLocation$;")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "Unknown", "()Lca/uwaterloo/flix/language/ast/package$SourceLocation;", false)
      visitor.visitMethodInsn(INVOKESPECIAL, "ca/uwaterloo/flix/api/MatchException", "<init>", "(Ljava/lang/String;Lca/uwaterloo/flix/language/ast/package$SourceLocation;)V", false)
      visitor.visitInsn(ATHROW)

    case Expression.SwitchError(_, loc) =>
      visitor.visitTypeInsn(NEW, "ca/uwaterloo/flix/api/SwitchException")
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(s"Non-exhaustive switch expression: ${loc.format}.")
      // TODO: Load actual source location or change SwitchException
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "MODULE$", "Lca/uwaterloo/flix/language/ast/package$SourceLocation$;")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/language/ast/package$SourceLocation$", "Unknown", "()Lca/uwaterloo/flix/language/ast/package$SourceLocation;", false)
      visitor.visitMethodInsn(INVOKESPECIAL, "ca/uwaterloo/flix/api/SwitchException", "<init>", "(Ljava/lang/String;Lca/uwaterloo/flix/language/ast/package$SourceLocation;)V", false)
      visitor.visitInsn(ATHROW)
  }

  /*
   * Some types (e.g. bool, int, str) need to be boxed as Flix values.
   * Other types (e.g. tag, tuple) are already Flix values.
   */
  private def compileBoxedExpr(ctx: Context, visitor: MethodVisitor)(exp: Expression): Unit = exp.tpe match {
    case Type.Bool =>
      // If we know the value of the boolean expression, then compile it directly rather than calling mkBool.
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      exp match {
        case Expression.True => visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "True", "()Ljava/lang/Object;", false)
        case Expression.False => visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "False", "()Ljava/lang/Object;", false)
        case _ =>
          compileExpression(ctx, visitor)(exp)
          visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkBool", "(Z)Ljava/lang/Object;", false)
      }

    case Type.Char =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkChar", "(C)Ljava/lang/Object;", false)

    case Type.Float32 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkFloat32", "(F)Ljava/lang/Object;", false)

    case Type.Float64 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkFloat64", "(D)Ljava/lang/Object;", false)

    case Type.Int8 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt8", "(I)Ljava/lang/Object;", false)

    case Type.Int16 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt16", "(I)Ljava/lang/Object;", false)

    case Type.Int32 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt32", "(I)Ljava/lang/Object;", false)

    case Type.Int64 =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkInt64", "(J)Ljava/lang/Object;", false)

    case Type.Str =>
      visitor.visitFieldInsn(GETSTATIC, "ca/uwaterloo/flix/runtime/Value$", "MODULE$", "Lca/uwaterloo/flix/runtime/Value$;")
      compileExpression(ctx, visitor)(exp)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$", "mkStr", "(Ljava/lang/String;)Ljava/lang/Object;", false)

    case Type.Unit | Type.BigInt | Type.Enum(_, _) | Type.Tuple(_) | Type.FSet(_) => compileExpression(ctx, visitor)(exp)

    case Type.Tag(_, _, _) => throw InternalCompilerException(s"Can't have a value of type ${exp.tpe}.")

  }

  /*
   * The value at the top of the stack is boxed as a Flix Value, and needs to be unboxed (e.g. to an int, boolean, or
   * String), or it needs to be cast to a specific Value type (e.g. Value.Unit.type, Value.Tag).
   *
   * Note that the generated code here is slightly more efficient than calling `cast2XX` since we don't have to branch.
   */
  private def compileUnbox(ctx: Context, visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
    case Type.Unit => visitor.visitTypeInsn(CHECKCAST, "ca/uwaterloo/flix/runtime/Value$Unit$")

    case Type.Bool =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)

    case Type.Char =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Character")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)

    case Type.Float32 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Float")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false)

    case Type.Float64 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Double")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false)

    case Type.Int8 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Byte")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Byte", "byteValue", "()B", false)

    case Type.Int16 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Short")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)

    case Type.Int32 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Integer")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)

    case Type.Int64 =>
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Long")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)

    case Type.BigInt => visitor.visitTypeInsn(CHECKCAST, "java/math/BigInteger")

    case Type.Str => visitor.visitTypeInsn(CHECKCAST, "java/lang/String")

    case Type.Enum(_, _) => visitor.visitTypeInsn(CHECKCAST, "ca/uwaterloo/flix/runtime/Value$Tag")

    case Type.Tuple(_) => visitor.visitTypeInsn(CHECKCAST, "ca/uwaterloo/flix/runtime/Value$Tuple")

    case Type.FSet(_) => visitor.visitTypeInsn(CHECKCAST, "scala/collection/immutable/Set")

    case Type.Tag(_, _, _) => throw InternalCompilerException(s"Can't have a value of type $tpe.")

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
  private def compileLoadExpr(ctx: Context, visitor: MethodVisitor)(load: LoadExpression): Unit = {
    compileExpression(ctx, visitor)(load.e)
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
  private def compileStoreExpr(ctx: Context, visitor: MethodVisitor)(store: StoreExpression): Unit = {
    compileExpression(ctx, visitor)(store.e)
    compileInt(visitor)(store.targetMask, isLong = true)
    visitor.visitInsn(LAND)
    compileExpression(ctx, visitor)(store.v)
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

  private def compileUnaryExpr(ctx: Context, visitor: MethodVisitor)(op: UnaryOperator, e: Expression): Unit = {
    compileExpression(ctx, visitor)(e)
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
    case Type.BigInt => visitor.visitMethodInsn(INVOKEVIRTUAL, "java/math/BigInteger", "negate", "()Ljava/math/BigInteger;", false);
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
    case Type.BigInt => visitor.visitMethodInsn(INVOKEVIRTUAL, "java/math/BigInteger", "not", "()Ljava/math/BigInteger;", false);
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
  private def compileArithmeticExpr(ctx: Context, visitor: MethodVisitor)
                                   (o: ArithmeticOperator, e1: Expression, e2: Expression): Unit = {
    if (o == BinaryOperator.Exponentiate) {
      val (castToDouble, castFromDouble) = e1.tpe match {
        case Type.Float32 => (F2D, D2F)
        case Type.Float64 => (NOP, NOP) // already a double
        case Type.Int8 | Type.Int16 | Type.Int32 => (I2D, D2I)
        case Type.Int64 => (L2D, D2L)
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
      }
      visitor.visitFieldInsn(GETSTATIC, "scala/math/package$", "MODULE$", "Lscala/math/package$;")
      compileExpression(ctx, visitor)(e1)
      visitor.visitInsn(castToDouble)
      compileExpression(ctx, visitor)(e2)
      visitor.visitInsn(castToDouble)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "scala/math/package$", "pow", "(DD)D", false)
      visitor.visitInsn(castFromDouble)
      (e1.tpe: @unchecked) match {
        case Type.Int8 => visitor.visitInsn(I2B)
        case Type.Int16 => visitor.visitInsn(I2S)
        case Type.Float32 | Type.Float64 | Type.Int32 | Type.Int64 => visitor.visitInsn(NOP)
      }
    } else {
      compileExpression(ctx, visitor)(e1)
      compileExpression(ctx, visitor)(e2)
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
        case Type.BigInt => visitor.visitMethodInsn(INVOKEVIRTUAL, "java/math/BigInteger", bigIntOp, "(Ljava/math/BigInteger;)Ljava/math/BigInteger;", false);
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
      }
    }
  }

  /*
   * Ints, Floats, and Chars support all six comparison operations (LE, LT, GE, GT, EQ, NE), but Unit, Bools, Strings,
   * Enums, Tuples, and Sets only support EQ and NE. Note that the generated code uses the negated condition, i.e.
   * branch if the (source) condition is false.
   *
   * Some reference types (Unit, String, and Enum) can use reference equality because of interning.
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
  private def compileComparisonExpr(ctx: Context, visitor: MethodVisitor)
                                   (o: ComparisonOperator, e1: Expression, e2: Expression): Unit = {
    e1.tpe match {
      case Type.Tuple(_) | Type.FSet(_) if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
        (e1.tpe: @unchecked) match {
          case Type.Tuple(_) =>
            // We know it's a tuple, so directly call java.util.Arrays.equals
            compileExpression(ctx, visitor)(e1)
            visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tuple", "elms", "()[Ljava/lang/Object;", false)
            compileExpression(ctx, visitor)(e2)
            visitor.visitMethodInsn(INVOKEVIRTUAL, "ca/uwaterloo/flix/runtime/Value$Tuple", "elms", "()[Ljava/lang/Object;", false)
            visitor.visitMethodInsn(INVOKESTATIC, "java/util/Arrays", "equals", "([Ljava/lang/Object;[Ljava/lang/Object;)Z", false)
          case Type.FSet(_) =>
            // Call the general java.lang.Object.equals
            compileExpression(ctx, visitor)(e1)
            compileExpression(ctx, visitor)(e2)
            visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false)
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
      case Type.Tag(_, _, _) => throw InternalCompilerException(s"Can't have a value of type ${e1.tpe}.")
      case _ =>
        compileExpression(ctx, visitor)(e1)
        compileExpression(ctx, visitor)(e2)
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
          case Type.Unit | Type.Str | Type.Enum(_, _) if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // Unit, String, and Enum can be reference compared for equality.
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
            visitor.visitMethodInsn(INVOKEVIRTUAL, "java/math/BigInteger", "compareTo", "(Ljava/math/BigInteger;)I", false)
            visitor.visitInsn(ICONST_0)
            visitor.visitJumpInsn(intOp, condElse)
          case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
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
  private def compileLogicalExpr(ctx: Context, visitor: MethodVisitor)
                                (o: LogicalOperator, e1: Expression, e2: Expression): Unit = o match {
    case BinaryOperator.LogicalAnd =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(ctx, visitor)(e1)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(ctx, visitor)(e2)
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
      compileExpression(ctx, visitor)(e1)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(ctx, visitor)(e2)
      visitor.visitJumpInsn(IFEQ, orFalseBranch)
      visitor.visitLabel(orTrueBranch)
      visitor.visitInsn(ICONST_1)
      visitor.visitJumpInsn(GOTO, orEnd)
      visitor.visitLabel(orFalseBranch)
      visitor.visitInsn(ICONST_0)
      visitor.visitLabel(orEnd)
    case BinaryOperator.Implication =>
      // (e1 ==> e2) === (!e1 || e2)
      val notExp = Expression.Unary(UnaryOperator.LogicalNot, e1, Type.Bool, e1.loc)
      compileLogicalExpr(ctx, visitor)(BinaryOperator.LogicalOr, notExp, e2)
    case BinaryOperator.Biconditional =>
      // (e1 <==> e2) === (e1 == e2)
      compileComparisonExpr(ctx, visitor)(BinaryOperator.Equal, e1, e2)
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
  private def compileBitwiseExpr(ctx: Context, visitor: MethodVisitor)
                                (o: BitwiseOperator, e1: Expression, e2: Expression): Unit = {
    compileExpression(ctx, visitor)(e1)
    compileExpression(ctx, visitor)(e2)
    val (tpe1, tpe2) = ("(Ljava/math/BigInteger;)Ljava/math/BigInteger;", "(I)Ljava/math/BigInteger;")
    val (intOp, longOp, bigintOp, bigintTpe) = o match {
      case BinaryOperator.BitwiseAnd => (IAND, LAND, "and", tpe1)
      case BinaryOperator.BitwiseOr => (IOR, LOR, "or", tpe1)
      case BinaryOperator.BitwiseXor => (IXOR, LXOR, "xor", tpe1)
      case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL, "shiftLeft", tpe2)
      case BinaryOperator.BitwiseRightShift => (ISHR, LSHR, "shiftRight", tpe2)
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
      case Type.BigInt => visitor.visitMethodInsn(INVOKEVIRTUAL, "java/math/BigInteger", bigintOp, bigintTpe, false);
      case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
    }
  }

}
