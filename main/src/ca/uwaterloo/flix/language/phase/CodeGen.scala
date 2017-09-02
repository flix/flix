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

import java.lang.reflect.Modifier

import ca.uwaterloo.flix.api.{Flix, MatchException, SwitchException, UserException}
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Hook
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException, Options, Validation}
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.CheckClassAdapter
import org.objectweb.asm.{Type => _, _}
import ca.uwaterloo.flix.util.Validation._
import CodegenHelper._

import scala.language.existentials

object CodeGen extends Phase[ExecutableAst.Root, ExecutableAst.Root] {

  // This constant is used in LoadBytecode, so we can't put it in the private Constants object.
  val flixObject = "flixObject"

  /**
    * Generate bytecode of expression in Flix.
    * There are a number of steps we take before and after the actual code generation.
    *
    * 1. Group constants and transform non-functions.
    * We group all constants by their prefixes to determine which methods are compiled into which classes. Also, we
    * transform all non-function constants into 0-arg functions, since codegen only compiles methods.
    * Example 1: given a root with constants A.B.C/f, A.B/g, A.B.C/h, we want to generate two classes, A.B.C
    * (containing methods f and h) and A.B (containing method g).
    * Example 2: (in pseudocode) the constant `def x = UserError` is converted to `def x() = UserError`.
    *
    * 2. Create a declarations map of names to types.
    * We need to know the type of function f in order to generate code that calls f.
    *
    * 3. Create Enum Type info
    * We create type information for each enum case. That is, given the type and tag name of each enum case, the map
    * returns the Qualified name of the the class corresponding to the enum case.
    *
    * 4. Generate functional interfaces.
    * Our implementation of closures requires the lambda function to be called through an interface (which is
    * annotated with @FunctionalInterface). Instead of using functional interfaces provided by Java or Scala (which
    * are too specific or too general), we create our own.
    * In this step, we generate names for the functional interfaces, placing each interface in the package
    *    ca.uwaterloo.flix.runtime. We iterate over the entire AST to determine which function types are used in
    * MkClosureRef, remove duplicate types, and then generate names. We want the type of MkClosureRef, which is the
    * type of the closure, not the type of the lambda, since its underlying implementation method will have its
    * argument list modified for capture variables.
    * Note that this includes synthetic functions that were lambda lifted, as well as user-defined functions being
    * passed around as closures.
    * Finally, we generate bytecode for each name. We keep the types and names in an interfaces map, so that given
    * a closure's signature, we can lookup the functional interface it's called through.
    *
    * 5. Generate bytecode for classes.
    * As of this step, we have grouped the constants into separate classes, transformed non-functions into 0-arg
    * functions, collected all the declarations in a map, and created functional interfaces and collected
    * them in a map.
    */
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    if (flix.options.evaluation == Evaluation.Interpreted) {
      return root.toSuccess
    }

    // 1. Group constants and transform non-functions.
    val constantsMap: Map[FlixClassName, List[ExecutableAst.Def]] = root.defs.values.map { f =>
      f.tpe match {
        case Type.Apply(Type.Arrow(l), _) => f
        case t => f.copy(tpe = Type.mkArrow(List(), t))
      }
    }.toList.groupBy(cst => FlixClassName(cst.sym.prefix))
    // TODO: Here we filter laws, since the backend does not support existentials/universals, but could we fix that?
    val constantsList: List[ExecutableAst.Def] = constantsMap.values.flatten.toList.filterNot(_.ann.isLaw)

    // 2. Create the declarations map.
    val declarations: Map[Symbol.DefnSym, Type] = constantsList.map(f => f.sym -> f.tpe).toMap

    // 3. Create Enum Type info
    val allEnums: List[(Type, (String, Type))] = root.defs.values.flatMap(x => CodegenHelper.findEnumCases(x.exp)).toList

    val enumTypeInfo: Map[(Type, String), (QualName, ExecutableAst.Case)] = allEnums.map { case (tpe, (name, subType)) =>
      val sym = tpe match {
        case Type.Apply(Type.Enum(s, _), _) => s
        case Type.Enum(s, _) => s
        case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }
      val enumCase = root.enums(sym).cases(name)
      (tpe, name) -> (EnumClassName(sym, name, typeToWrappedType(subType)), enumCase)
    }.toMap

    // 4. Generate functional interfaces.
    val interfaceNames: Map[Type, FlixClassName] = CodegenHelper.generateInterfaceNames(constantsList)

    val interfaceByteCodes: Map[Type, (FlixClassName, Array[Byte])] = interfaceNames.map { case (tpe, prefix) =>
      // Use a temporary context with no functions, because the codegen needs the map of interfaces.
      val bytecode = CodeGen.compileFunctionalInterface(prefix, declarations, interfaceNames)(tpe)
      tpe -> (prefix, bytecode)
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 5. Generate bytecode for classes.
    val classByteCodes: Map[FlixClassName, Array[Byte]] = constantsMap.map { case (prefix, consts) =>
      val bytecode = CodeGen.compile(prefix, consts, declarations, interfaceNames, enumTypeInfo, flix.options)
      prefix -> bytecode
    }.toMap // Despite IDE highlighting, this is actually necessary.

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(codeGen = e),
      byteCodes = root.byteCodes.copy(functionalInterfaceByteCodes = interfaceByteCodes, classByteCodes = classByteCodes)).toSuccess
  }

  /*
   * Compile an interface with a single abstract method `apply` whose signature matches the given type. Furthermore, we
   * annotate the interface with @FunctionalInterface.
   */
  def compileFunctionalInterface(prefix: QualName,
                                 declarations: Map[Symbol.DefnSym, Type],
                                 interfaces: Map[Type, FlixClassName])(tpe: Type): Array[Byte] = {
    val visitor = new ClassWriter(0)
    visitor.visit(JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, decorate(prefix), null,
      asm.Type.getInternalName(Constants.objectClass), null)

    // Add annotation @java.lang.FunctionalInterface
    val av = visitor.visitAnnotation(asm.Type.getDescriptor(classOf[java.lang.FunctionalInterface]), true)
    av.visitEnd()

    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "apply", descriptor(tpe, interfaces), null, null)
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
  def compile(prefix: QualName,
              functions: List[ExecutableAst.Def],
              declarations: Map[Symbol.DefnSym, Type],
              interfaces: Map[Type, FlixClassName],
              enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
              options: Options): Array[Byte] = {
    /*
     * Initialize the class writer. We override `getCommonSuperClass` method because `asm` implementation of this
     * function requires types to loaded so that they can be compared to each other.
     */
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
      override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
        asm.Type.getInternalName(Constants.objectClass)
      }
    }

    // Wrap the class writer in a CheckClassAdapter if compiler invariants are enabled.
    val visitor =
      if (options.invariants)
        new CheckClassAdapter(classWriter)
      else
        classWriter

    // Initialize the visitor to create a class.
    visitor.visit(JavaVersion, ACC_PUBLIC + ACC_SUPER, decorate(prefix), null,
      asm.Type.getInternalName(Constants.objectClass), null)

    // Source of the class
    visitor.visitSource(baseFileName(prefix), null)

    compileStaticFlixField(prefix, visitor)
    compileConstructor(visitor)
    // TODO: Here we filter laws, since the backend does not support existentials/universals, but could we fix that?
    functions.filterNot(_.ann.isLaw).foreach(compileFunction(prefix, functions, declarations, interfaces, enums, visitor))

    visitor.visitEnd()

    classWriter.toByteArray
  }

  /*
   * Create a static field for the Flix object, and generate the class initializer to initialize the field to null.
   */
  private def compileStaticFlixField(prefix: QualName, visitor: ClassVisitor): Unit = {
    val fv = visitor.visitField(ACC_PUBLIC + ACC_STATIC, flixObject, asm.Type.getDescriptor(Constants.flixClass), null, null)
    fv.visitEnd()

    val mv = visitor.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    mv.visitCode()
    mv.visitInsn(ACONST_NULL)
    mv.visitFieldInsn(PUTSTATIC, decorate(prefix), flixObject, asm.Type.getDescriptor(Constants.flixClass))
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 0)
    mv.visitEnd()
  }

  /*
   * Generate the constructor. Takes a Context and an initialized ClassVisitor.
   */
  private def compileConstructor(visitor: ClassVisitor): Unit = {
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
  private def compileFunction(prefix: QualName,
                              functions: List[ExecutableAst.Def],
                              declarations: Map[Symbol.DefnSym, Type],
                              interfaces: Map[Type, FlixClassName],
                              enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                              visitor: ClassVisitor)(function: ExecutableAst.Def): Unit = {
    val flags = if (function.isSynthetic) ACC_PUBLIC + ACC_STATIC + ACC_SYNTHETIC else ACC_PUBLIC + ACC_STATIC
    val mv = visitor.visitMethod(flags, function.sym.suffix, descriptor(function.tpe, interfaces), null, null)
    mv.visitCode()

    val entryPoint = new Label()
    mv.visitLabel(entryPoint)

    compileExpression(prefix, functions, declarations, interfaces, enums, mv, entryPoint)(function.exp)

    val tpe = function.tpe match {
      case Type.Apply(Type.Arrow(l), ts) => ts.last
      case _ => throw InternalCompilerException(s"Constant ${function.sym} should have been converted to a function.")
    }

    tpe match {
      case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => mv.visitInsn(IRETURN)
      case Type.Int64 => mv.visitInsn(LRETURN)
      case Type.Float32 => mv.visitInsn(FRETURN)
      case Type.Float64 => mv.visitInsn(DRETURN)
      case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.Tuple(_), _) | Type.Apply(Type.Arrow(_), _) |
           Type.Apply(_, _) => mv.visitInsn(ARETURN)
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    // Dummy large numbers (JVM limits) so the bytecode checker can run. Afterwards, the ASM library calculates the proper maxes.
    mv.visitMaxs(65535, 65535)
    mv.visitEnd()
  }

  private def compileExpression(prefix: QualName,
                                functions: List[ExecutableAst.Def],
                                declarations: Map[Symbol.DefnSym, Type],
                                interfaces: Map[Type, FlixClassName],
                                enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                                visitor: MethodVisitor,
                                entryPoint: Label)(expr: Expression): Unit = expr match {
    case Expression.Unit =>
      val clazz = Constants.unitClass
      val method = clazz.getMethod("getInstance")
      visitor.visitMethodInsn(INVOKESTATIC, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
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

    case Expression.Var(sym, tpe, _) => tpe match {
      case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ILOAD, sym.getStackOffset)
      case Type.Int64 => visitor.visitVarInsn(LLOAD, sym.getStackOffset)
      case Type.Float32 => visitor.visitVarInsn(FLOAD, sym.getStackOffset)
      case Type.Float64 => visitor.visitVarInsn(DLOAD, sym.getStackOffset)
      case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.Arrow(_), _) |
           Type.Apply(Type.Enum(_, _), _) => visitor.visitVarInsn(ALOAD, sym.getStackOffset)
      case _ if tpe.isTuple | tpe.isRef => visitor.visitVarInsn(ALOAD, sym.getStackOffset)
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
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
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(v)
      }

      // The name of the method implemented by the lambda.
      val invokedName = "apply"

      // The type descriptor of the CallSite. Its arguments are the types of capture variables, and its return
      // type is the interface the lambda object implements (i.e. the type of the closure).
      val csTpe = Type.mkArrow(freeVars.toList.map(_.tpe), tpe)
      val invokedType = descriptor(csTpe, interfaces)

      // The handle for the bootstrap method we pass to InvokeDynamic, which is
      // `java.lang.invoke.LambdaMetafactory.metafactory(...)`.
      val clazz = classOf[java.lang.invoke.LambdaMetafactory]
      val method = clazz.getMethods.filter(m => m.getName == "metafactory").head
      val bsmHandle = new Handle(H_INVOKESTATIC, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)

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
      val samMethodType = asm.Type.getType(descriptor(tpe, interfaces))
      val implMethod = new Handle(H_INVOKESTATIC, decorate(FlixClassName(sym.prefix)), sym.suffix, descriptor(fnType, interfaces), false)
      val instantiatedMethodType = asm.Type.getType(descriptor(tpe, interfaces))
      val bsmArgs = Array(samMethodType, implMethod, instantiatedMethodType)

      // Finally, generate the InvokeDynamic instruction.
      visitor.visitInvokeDynamicInsn(invokedName, invokedType, bsmHandle, bsmArgs: _*)

    case Expression.ApplyClo(exp, args, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Lambdas are called through an interface. We don't know what function we're calling, but we know its type,
      // so we can lookup the interface we're calling through.
      val name = interfaces(exp.tpe)

      // Evaluate the function we're calling.
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the interface call.
      args.foreach(compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint))
      visitor.visitMethodInsn(INVOKEINTERFACE, decorate(name), "apply", descriptor(exp.tpe, interfaces), true)

    case Expression.ApplyDef(name, args, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We know what function we're calling, so we can look up its signature.
      val targetTpe = declarations(name)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the call.
      args.foreach(compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint))
      visitor.visitMethodInsn(INVOKESTATIC, decorate(FlixClassName(name.prefix)), name.suffix, descriptor(targetTpe, interfaces), false)

    case Expression.ApplyCloTail(exp, args, _, loc) =>
      // TODO: Duplicated from Expression.ApplyClo. Pending rewrite related to IFOs.

      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Lambdas are called through an interface. We don't know what function we're calling, but we know its type,
      // so we can lookup the interface we're calling through.
      val name = interfaces(exp.tpe)

      // Evaluate the function we're calling.
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the interface call.
      args.foreach(compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint))
      visitor.visitMethodInsn(INVOKEINTERFACE, decorate(name), "apply", descriptor(exp.tpe, interfaces), true)

    case Expression.ApplyDefTail(name, args, _, loc) =>
      // TODO: Duplicated from Expression.ApplyDef. Pending rewrite related to IFOs.

      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We know what function we're calling, so we can look up its signature.
      val targetTpe = declarations(name)

      // Evaluate arguments left-to-right and push them onto the stack. Then make the call.
      args.foreach(compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint))
      visitor.visitMethodInsn(INVOKESTATIC, decorate(FlixClassName(name.prefix)), name.suffix, descriptor(targetTpe, interfaces), false)

    case Expression.ApplySelfTail(name, formals, actuals, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate each argument and push the result on the stack.
      // Compute the stack height taken by the arguments based on their types.
      var globalOffset: Int = 0
      for (arg <- actuals) {
        // Evaluate the argument and push the result on the stack.
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(arg)

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

    case Expression.ApplyHook(hook, args, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val (isSafe, name, elmsClass) = hook match {
        case _: Hook.Unsafe => (false, "invokeUnsafe", Constants.objectClass)
      }
      val clazz = Constants.flixClass
      val method = clazz.getMethods.filter(m => m.getName == name).head

      // First we get the Flix object from the static field.
      visitor.visitFieldInsn(GETSTATIC, decorate(prefix), flixObject, asm.Type.getDescriptor(clazz))

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

        compileBoxedExpr(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e)
        visitor.visitInsn(AASTORE)
      }

      // Finally, make the virtual call to invoke/invokeUnsafe. If it's a safe hook, we also need to call `getUnsafeRef`.
      visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
      if (isSafe) {
        val method2 = elmsClass.getMethod("getUnsafeRef")
        visitor.visitMethodInsn(INVOKEINTERFACE, asm.Type.getInternalName(elmsClass), method2.getName, asm.Type.getMethodDescriptor(method2), true)
      }

      // Unbox the result, if necessary.
      compileUnbox(interfaces, visitor)(tpe)

    case Expression.Unary(sop, op, exp, _, _) => compileUnaryExpr(prefix, functions, declarations, interfaces, enums,
      visitor, entryPoint)(op, exp)
    case Expression.Binary(sop, op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => compileArithmeticExpr(prefix, functions, declarations, interfaces, enums,
        visitor, entryPoint)(o, exp1, exp2)
      case o: ComparisonOperator => compileComparisonExpr(prefix, functions, declarations, interfaces, enums,
        visitor, entryPoint)(o, exp1, exp2)
      case o: LogicalOperator => compileLogicalExpr(prefix, functions, declarations, interfaces, enums,
        visitor, entryPoint)(o, exp1, exp2)
      case o: BitwiseOperator => compileBitwiseExpr(prefix, functions, declarations, interfaces, enums, visitor,
        entryPoint)(o, exp1, exp2)
    }

    case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp1)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp2)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp3)
      visitor.visitLabel(ifEnd)

    case Expression.Branch(exp, branches, tpe, loc) =>
      ??? // TODO

    case Expression.JumpTo(sym, tpe, loc) =>
      ??? // TODO

    case Expression.Let(sym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp1)
      exp1.tpe match {
        case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id.")
        case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 => visitor.visitVarInsn(ISTORE, sym.getStackOffset)
        case Type.Int64 => visitor.visitVarInsn(LSTORE, sym.getStackOffset)
        case Type.Float32 => visitor.visitVarInsn(FSTORE, sym.getStackOffset)
        case Type.Float64 => visitor.visitVarInsn(DSTORE, sym.getStackOffset)
        case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.Tuple(_), _) | Type.Apply(Type.Arrow(_), _) => visitor.visitVarInsn(ASTORE, sym.getStackOffset)
        case Type.Apply(_, _) => visitor.visitVarInsn(ASTORE, sym.getStackOffset)
        case tpe => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp2)

    case Expression.LetRec(sym, exp1, exp2, _, _) =>
      ??? // TODO: Add support for LetRec.

    case Expression.Is(enum, tag, exp, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // First we compile the `exp`
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
      // We look in the enum map to find the qualified name of the class of the enum case
      val clazz = enums(exp.tpe, tag)._1
      // We check if the enum is `instanceof` the class
      visitor.visitTypeInsn(INSTANCEOF, decorate(clazz))

    case Expression.Tag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      //  We look in the enum map to find the qualified name of the class of the enum case
      val (clazzName, enumCase) = enums(tpe, tag)
      /*
       If the definition of the enum case has a `Unit` field, then it is represented by singleton pattern which means
       there is only one instance of the class initiated as a field. We have to fetch this field instead of instantiating
       a new one.
       */
      if (isSingletonEnum(enumCase)) {
        visitor.visitFieldInsn(GETSTATIC, decorate(clazzName), "unitInstance", s"L${decorate(clazzName)};")
      } else {
        /*
         * We get the descriptor of the type of the `value` field of enum, if type if primitive we use the corresponding
         * primitive in java otherwise we use the descriptor of object.
         */
        val desc = getWrappedTypeDescriptor(typeToWrappedType(exp.tpe))
        // Creating a new instance of the class
        visitor.visitTypeInsn(NEW, decorate(clazzName))
        visitor.visitInsn(DUP)
        // Evaluating the single argument of the class constructor
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        // Calling the constructor of the class
        visitor.visitMethodInsn(INVOKESPECIAL, decorate(clazzName), "<init>", s"(${desc})V", false)
      }
    case Expression.Untag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      /*
       * We get the descriptor of the type of the `value` field of enum, if type if primitive we use the corresponding
       * primitive in java otherwise we use the descriptor of object.
       */
      val desc = getWrappedTypeDescriptor(typeToWrappedType(tpe))
      // Qualified name of the enum
      val clazz = EnumClassName(enum, tag, typeToWrappedType(tpe))
      // Evaluate the exp
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
      // Cast the exp to the type of the tag
      visitor.visitTypeInsn(CHECKCAST, decorate(clazz))
      // Invoke `getValue()` method to extract the field of the tag
      visitor.visitMethodInsn(INVOKEVIRTUAL, decorate(clazz), "getValue", s"()$desc", false)
      // Cast the object to it's type if it's not a primitive
      castIfNotPrim(tpe, interfaces, visitor)

    case Expression.Index(base, offset, tpe, _) =>
      // Descriptor of the field of the element in the tuple specified by the `offset`
      val desc = getWrappedTypeDescriptor(typeToWrappedType(tpe))
      // Qualified name of the class defining the tuple
      val clazzName = base.tpe match {
        case Type.Apply(Type.Tuple(_), lst) => TupleClassName(lst.map(typeToWrappedType))
        case _ => throw InternalCompilerException(s"Unexpected type: `${base.tpe}`")
      }
      // evaluating the `base`
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(base)
      // Invoking `getField${offset}()` method for fetching the field
      visitor.visitMethodInsn(INVOKEVIRTUAL, decorate(clazzName), s"getIndex$offset", s"()$desc", false)
      // Cast the object to it's type if it's not a primitive
      castIfNotPrim(tpe, interfaces, visitor)

    case Expression.Tuple(elms, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Transforming types of elements of the tuple to the appropriate type
      val fieldTypes: List[WrappedType] = elms.map(x => typeToWrappedType(x.tpe)).toList
      // Descriptor of the parameters of the constructor of the tuple class
      val desc = fieldTypes.map(getWrappedTypeDescriptor).mkString
      // Qualified name of the class that can represent this tuple
      val clazzName = TupleClassName(fieldTypes)
      // Creating a new instance of the class
      visitor.visitTypeInsn(NEW, decorate(clazzName))
      // Duplicating the class
      visitor.visitInsn(DUP)
      // Evaluating all the elements to be stored in the tuple class
      elms.foreach {
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(_)
      }
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, decorate(clazzName), "<init>", s"(${desc})V", false)

    case Expression.Ref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Class appropriate reference class
      val clazz = getReferenceClazz(tpe)
      // Get internal name of the class
      val clazzInternalName = asm.Type.getInternalName(clazz)
      // Get constructor of the class
      val constructorDescriptor = asm.Type.getConstructorDescriptor(clazz.getConstructors.last)
      // Create a new reference object
      visitor.visitTypeInsn(NEW, clazzInternalName)
      // Duplicate it since one instance will get consumed by constructor
      visitor.visitInsn(DUP)
      // Evaluate the underlying expression
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, clazzInternalName, "<init>", constructorDescriptor, false)

    case Expression.Deref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the exp
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
      // Class appropriate reference class
      val clazz = getReferenceClazz(exp.tpe)
      // Get internal name of the class
      val clazzInternalName = asm.Type.getInternalName(clazz)
      // Get descriptor of `getValue` method
      val methodDescriptor = asm.Type.getMethodDescriptor(clazz.getMethod("getValue"))
      // Dereference the expression
      visitor.visitMethodInsn(INVOKEVIRTUAL, clazzInternalName, "getValue", methodDescriptor, false)
      // Cast underlying value to the correct type if the underlying type is Object
      tpe match {
        case Type.Bool | Type.Char | Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 | Type.Float32 | Type.Float64 => // no need to cast primitives
        case _ =>
          val name = internalName(tpe, interfaces)
          visitor.visitTypeInsn(CHECKCAST, name)
      }

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the reference address
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp1)
      // Evaluating the value to be assigned to the reference
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp2)
      // Class appropriate reference class
      val clazz = getReferenceClazz(exp1.tpe)
      // Get internal name of the class
      val clazzInternalName = asm.Type.getInternalName(clazz)
      // Get descriptor of `setValue` method
      val methodDescriptor = asm.Type.getMethodDescriptor(clazz.getMethods.filter(m => m.getName == "setValue").last)
      // Invoke `setValue` method to set the value to the given number
      visitor.visitMethodInsn(INVOKEVIRTUAL, clazzInternalName, "setValue", methodDescriptor, false)
      // Since the return type is unit, we put an instance of unit on top of the stack
      val unitGetInstance = Constants.unitClass.getMethod("getInstance")
      visitor.visitMethodInsn(INVOKESTATIC, asm.Type.getInternalName(Constants.unitClass), unitGetInstance.getName,
        asm.Type.getMethodDescriptor(unitGetInstance), false)


    case Expression.Existential(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$expr' at ${loc.source.format}.")

    case Expression.Universal(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$expr' at ${loc.source.format}.")

    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)

      val descriptor = asm.Type.getConstructorDescriptor(constructor)
      val declaration = asm.Type.getInternalName(constructor.getDeclaringClass)
      // Create a new object of the declaration type
      visitor.visitTypeInsn(NEW, declaration)
      // Duplicate the reference since the first argument for a constructor call is the reference to the object
      visitor.visitInsn(DUP)
      // Evaluate arguments left-to-right and push them onto the stack.
      args.foreach(compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint))
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, declaration, "<init>", descriptor, false)

    case Expression.NativeField(field, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Fetch a field from an object
      val declaration = asm.Type.getInternalName(field.getDeclaringClass)
      val name = field.getName
      // Use GETSTATIC if the field is static and GETFIELD if the field is on an object
      val getInsn = if (Modifier.isStatic(field.getModifiers)) GETSTATIC else GETFIELD
      visitor.visitFieldInsn(getInsn, declaration, name, descriptor(tpe, interfaces))

    case Expression.NativeMethod(method, args, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate arguments left-to-right and push them onto the stack.
      args.foreach(compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint))
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)
      // If the method is static, use INVOKESTATIC otherwise use INVOKEVIRTUAL
      val invokeInsn = if (Modifier.isStatic(method.getModifiers)) INVOKESTATIC else INVOKEVIRTUAL
      visitor.visitMethodInsn(invokeInsn, declaration, name, descriptor, false)
      // If the method is void, put a unit on top of the stack
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
        val clazz = Constants.unitClass
        val method = clazz.getMethod("getInstance")
        visitor.visitMethodInsn(INVOKESTATIC, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
      }

    case Expression.UserError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val name = asm.Type.getInternalName(classOf[UserException])
      val msg = s"User exception: ${loc.format}."
      compileException(visitor, name, msg)

    case Expression.MatchError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val name = asm.Type.getInternalName(classOf[MatchException])
      val msg = s"Non-exhaustive match expression: ${loc.format}."
      compileException(visitor, name, msg)

    case Expression.SwitchError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
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
  private def compileBoxedExpr(prefix: QualName,
                               functions: List[ExecutableAst.Def],
                               declarations: Map[Symbol.DefnSym, Type],
                               interfaces: Map[Type, FlixClassName],
                               enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                               visitor: MethodVisitor,
                               entryPoint: Label)(exp: Expression): Unit = {
    // Adding source line number for debugging
    addSourceLine(visitor, exp.loc)

    exp.tpe match {
      case Type.Bool =>
        val booleanInternalName = "java/lang/Boolean"
        // If we know the value of the boolean expression, then compile it directly rather than calling constructor of boolean class
        exp match {
          case Expression.True => visitor.visitFieldInsn(GETSTATIC, booleanInternalName, "TRUE", s"L$booleanInternalName;")
          case Expression.False => visitor.visitFieldInsn(GETSTATIC, booleanInternalName, "FALSE", s"L$booleanInternalName;")
          case _ =>
            visitor.visitTypeInsn(NEW, booleanInternalName)
            visitor.visitInsn(DUP)
            compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
            visitor.visitMethodInsn(INVOKESPECIAL, booleanInternalName, "<init>", "(Z)V", false)
        }

      case Type.Char =>
        val charInternalName = "java/lang/Character"
        visitor.visitTypeInsn(NEW, charInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, charInternalName, "<init>", "(C)V", false)

      case Type.Float32 =>
        val floatInternalName = "java/lang/Float"
        visitor.visitTypeInsn(NEW, floatInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, floatInternalName, "<init>", "(F)V", false)

      case Type.Float64 =>
        val doubleInternalName = "java/lang/Double"
        visitor.visitTypeInsn(NEW, doubleInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, doubleInternalName, "<init>", "(D)V", false)

      case Type.Int8 =>
        val byteInternalName = "java/lang/Byte"
        visitor.visitTypeInsn(NEW, byteInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, byteInternalName, "<init>", "(B)V", false)

      case Type.Int16 =>
        val shortInternalName = "java/lang/Short"
        visitor.visitTypeInsn(NEW, shortInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, shortInternalName, "<init>", "(S)V", false)

      case Type.Int32 =>
        val intInternalName = "java/lang/Integer"
        visitor.visitTypeInsn(NEW, intInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, intInternalName, "<init>", "(I)V", false)

      case Type.Int64 =>
        val longInternalName = "java/lang/Long"
        visitor.visitTypeInsn(NEW, longInternalName)
        visitor.visitInsn(DUP)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)
        visitor.visitMethodInsn(INVOKESPECIAL, longInternalName, "<init>", "(J)V", false)

      case Type.Unit | Type.BigInt | Type.Str | Type.Native | Type.Enum(_, _) | Type.Apply(Type.Tuple(_), _) | Type.Apply(Type.Arrow(_), _) |
           Type.Apply(Type.Enum(_, _), _) => compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(exp)

      case tpe => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }
  }

  /*
   * The value at the top of the stack is boxed as a Flix Value, and needs to be unboxed (e.g. to an int, boolean, or
   * String), or it needs to be cast to a specific Value type (e.g. Value.Unit.type, Value.Tag).
   *
   * Note that the generated code here is slightly more efficient than calling `cast2XX` since we don't have to branch.
   */
  private def compileUnbox(interfaces: Map[Type, FlixClassName], visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
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
        case Type.Enum(sym, _) => decorate(EnumInterfName(sym))
        case Type.Apply(Type.Enum(sym, _), _) => decorate(EnumInterfName(sym))
        case Type.Apply(Type.Arrow(l), _) =>
          // TODO: Is this correct? Need to write a test when we can write lambda expressions.
          decorate(interfaces(tpe))
        case _ => throw new InternalCompilerException(s"Type $tpe should not be handled in this case.")
      }
      visitor.visitTypeInsn(CHECKCAST, name)

    case Type.Native => // Don't need to cast AnyRef to anything

    case Type.Apply(Type.Tuple(l), lst) =>
      val clazzName = TupleClassName(lst.map(typeToWrappedType))


      visitor.visitTypeInsn(CHECKCAST, decorate(clazzName))

    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
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

  private def compileUnaryExpr(prefix: QualName,
                               functions: List[ExecutableAst.Def],
                               declarations: Map[Symbol.DefnSym, Type],
                               interfaces: Map[Type, FlixClassName],
                               enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                               visitor: MethodVisitor,
                               entryPoint: Label)(op: UnaryOperator, e: Expression): Unit = {
    // Adding source line number for debugging
    addSourceLine(visitor, e.loc)

    compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e)
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
      case UnaryOperator.Minus => compileUnaryMinusExpr(prefix, functions, declarations, interfaces, enums, visitor)(e.tpe)
      case UnaryOperator.BitwiseNegate => compileUnaryNegateExpr(visitor)(e.tpe)
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
  private def compileUnaryMinusExpr(prefix: QualName,
                                    functions: List[ExecutableAst.Def],
                                    declarations: Map[Symbol.DefnSym, Type],
                                    interfaces: Map[Type, FlixClassName],
                                    enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                                    visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
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
  private def compileUnaryNegateExpr(visitor: MethodVisitor)(tpe: Type): Unit = tpe match {
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
  private def compileArithmeticExpr(prefix: QualName,
                                    functions: List[ExecutableAst.Def],
                                    declarations: Map[Symbol.DefnSym, Type],
                                    interfaces: Map[Type, FlixClassName],
                                    enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                                    visitor: MethodVisitor,
                                    entryPoint: Label)(o: ArithmeticOperator, e1: Expression, e2: Expression): Unit = {
    if (o == BinaryOperator.Exponentiate) {
      val (castToDouble, castFromDouble) = e1.tpe match {
        case Type.Float32 => (F2D, D2F)
        case Type.Float64 => (NOP, NOP) // already a double
        case Type.Int8 | Type.Int16 | Type.Int32 => (I2D, D2I)
        case Type.Int64 => (L2D, D2L)
        case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe}.")
      }
      visitor.visitFieldInsn(GETSTATIC, Constants.scalaMathPkg, "MODULE$", s"L${Constants.scalaMathPkg};")
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
      visitor.visitInsn(castToDouble)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
      visitor.visitInsn(castToDouble)
      visitor.visitMethodInsn(INVOKEVIRTUAL, Constants.scalaMathPkg, "pow", "(DD)D", false)
      visitor.visitInsn(castFromDouble)
      (e1.tpe: @unchecked) match {
        case Type.Int8 => visitor.visitInsn(I2B)
        case Type.Int16 => visitor.visitInsn(I2S)
        case Type.Float32 | Type.Float64 | Type.Int32 | Type.Int64 => visitor.visitInsn(NOP)
      }
    } else {
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
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
        case Type.Str => (e2.tpe, o) match {
          case (Type.Str, BinaryOperator.Plus) =>
            val clazz = Constants.stringClass
            val method = clazz.getMethod("concat", Constants.stringClass)
            visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
          case _ => throw InternalCompilerException(s"Can't apply $o to type ${e1.tpe} near ${e1.loc.format}")
        }
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
  private def compileComparisonExpr(prefix: QualName,
                                    functions: List[ExecutableAst.Def],
                                    declarations: Map[Symbol.DefnSym, Type],
                                    interfaces: Map[Type, FlixClassName],
                                    enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                                    visitor: MethodVisitor, entryPoint: Label)
                                   (o: ComparisonOperator, e1: Expression, e2: Expression): Unit = {
    e1.tpe match {
      case Type.Enum(_, _) | Type.Apply(Type.Tuple(_), _) | Type.Apply(Type.Enum(_, _), _) if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
        (e1.tpe: @unchecked) match {
          case Type.Enum(_, _) | Type.Apply(Type.Enum(_, _), _) =>
            compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
            compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
            val clazz = Constants.objectClass
            val method = clazz.getMethod("equals", clazz)
            visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
          case Type.Apply(Type.Tuple(_), _) =>
            compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
            compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
            val clazz = Constants.objectClass
            val method = clazz.getMethod("equals", clazz)
            visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
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
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
        compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
        val condElse = new Label()
        val condEnd = new Label()
        val (intOp, floatOp, doubleOp, cmp) = o match {
          case BinaryOperator.Less => (IF_ICMPGE, FCMPG, DCMPG, IFGE)
          case BinaryOperator.LessEqual => (IF_ICMPGT, FCMPG, DCMPG, IFGT)
          case BinaryOperator.Greater => (IF_ICMPLE, FCMPL, DCMPL, IFLE)
          case BinaryOperator.GreaterEqual => (IF_ICMPLT, FCMPL, DCMPL, IFLT)
          case BinaryOperator.Equal => (IF_ICMPNE, FCMPG, DCMPG, IFNE)
          case BinaryOperator.NotEqual => (IF_ICMPEQ, FCMPG, DCMPG, IFEQ)
        }
        e1.tpe match {
          case Type.Unit if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // Unit can only be equal to unit, so objects are poped from the top of the stack
            visitor.visitInsn(POP)
            visitor.visitInsn(POP)
            // A unit value is always equal itself, so no need to branch.
            // A unit value is never unequal to itself, so always branch to else label.
            e2.tpe match {
              case Type.Unit if o == BinaryOperator.NotEqual => visitor.visitJumpInsn(GOTO, condElse)
              case Type.Unit if o == BinaryOperator.Equal =>
              case _ if o == BinaryOperator.Equal => visitor.visitJumpInsn(GOTO, condElse)
              case _ =>
            }
          case Type.Str if o == BinaryOperator.Equal || o == BinaryOperator.NotEqual =>
            // String can be compared using Object's `equal` method
            val clazz = Constants.objectClass
            val method = clazz.getMethod("equals", clazz)
            visitor.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), method.getName, asm.Type.getMethodDescriptor(method), false)
            visitor.visitInsn(ICONST_1)
            visitor.visitJumpInsn(intOp, condElse)
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
  private def compileLogicalExpr(prefix: QualName,
                                 functions: List[ExecutableAst.Def],
                                 declarations: Map[Symbol.DefnSym, Type],
                                 interfaces: Map[Type, FlixClassName],
                                 enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                                 visitor: MethodVisitor,
                                 entryPoint: Label)(o: LogicalOperator, e1: Expression, e2: Expression): Unit = o match {
    case BinaryOperator.LogicalAnd =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
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
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
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
  private def compileBitwiseExpr(prefix: QualName,
                                 functions: List[ExecutableAst.Def],
                                 declarations: Map[Symbol.DefnSym, Type],
                                 interfaces: Map[Type, FlixClassName],
                                 enums: Map[(Type, String), (QualName, ExecutableAst.Case)],
                                 visitor: MethodVisitor,
                                 entryPoint: Label)(o: BitwiseOperator, e1: Expression, e2: Expression): Unit = {
    compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e1)
    compileExpression(prefix, functions, declarations, interfaces, enums, visitor, entryPoint)(e2)
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

  /*
   * Adding the source of the line for debugging
   */
  private def addSourceLine(visitor: MethodVisitor, loc: SourceLocation): Unit = {
    val label = new Label()
    visitor.visitLabel(label)
    visitor.visitLineNumber(loc.beginLine, label)
  }

  /*
   * `tpe` is type of value on top of the stack. If the value is not primitive, then we cast it to it's specific type,
   * if the value is a primitive then since there is no boxing, then no casting is necessary.
   */
  private def castIfNotPrim(tpe: Type, interfaces: Map[Type, FlixClassName], visitor: MethodVisitor): Unit = tpe match {
    case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
    case Type.Unit => visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(Constants.unitClass))
    case Type.Bool => ()
    case Type.Char => ()
    case Type.Float32 => ()
    case Type.Float64 => ()
    case Type.Int8 => ()
    case Type.Int16 => ()
    case Type.Int32 => ()
    case Type.Int64 => ()
    case Type.BigInt => visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(Constants.bigIntegerClass))
    case Type.Str => visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(Constants.stringClass))
    case Type.Native => visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(Constants.objectClass))
    case Type.Apply(Type.Arrow(l), _) => visitor.visitTypeInsn(CHECKCAST, decorate(interfaces(tpe)))
    case Type.Apply(Type.Tuple(l), lst) =>
      val clazzName = TupleClassName(lst.map(typeToWrappedType))
      visitor.visitTypeInsn(CHECKCAST, decorate(clazzName))
    case _ if tpe.isEnum =>
      val sym = tpe match {
        case Type.Apply(Type.Enum(s, _), _) => s
        case Type.Enum(s, _) => s
        case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }

      val fullName = EnumInterfName(sym)
      visitor.visitTypeInsn(CHECKCAST, decorate(fullName))
    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }
}
