package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Def, FormalParam, FreeVar, Root}
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

import scala.collection.parallel.CollectionConverters._

/**
  * Generates byte code for the closure classes.
  */
object GenClosureClasses {

  /**
    * Returns the set of closures classes for the given set of definitions `defs`.
    */
  def gen(closures: Set[ClosureInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a closure class for each closure and collect the results in a map.
    //
    ParOps.parAgg(closures, Map.empty[JvmName, JvmClass])({
      case (macc, closure) =>
        val jvmType = JvmOps.getClosureClassType(closure)
        val jvmName = jvmType.name
        val bytecode = genByteCode(closure)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }, _ ++ _)
  }

  /**
    * Returns the byte code for the closure with the given symbol `sym` and free variables `freeVars`.
    *
    * For example, given the symbol `mkAdder` with type (Int, Int) -> Int and the free variable `x`, we create:
    *
    * Clo$mkAdder extends Fn1$Int$Int {
    * private int x;
    * private int arg0;
    * private int res;
    *
    * public Clo$mkAdder(int x) {
    *     this.x = x;
    * }
    *
    * public setArg0(int arg0) {
    *     this.arg0 = arg0;
    * }
    *
    * public int getResult() {
    * return this.res;
    * }
    *
    * public void apply(Context ctx) {
    *     this.res = this.x + this.arg0;
    * }
    * }
    *
    */
  private def genByteCode(closure: ClosureInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Args of the function
    val MonoType.Arrow(targs, tresult) = closure.tpe

    // `JvmType` of the interface for `closure.tpe`
    val functionInterface = JvmOps.getFunctionInterfaceType(closure.tpe)

    // The super interface.
    val superInterface = Array(functionInterface.name.toInternalName, JvmName.Runnable.toInternalName)

    // `JvmType` of the class for `defn`
    val classType = JvmOps.getClosureClassType(closure)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, superInterface)

    // Context at creation
    AsmOps.compileField(visitor, "creationContext", JvmType.Object, isStatic = false, isPrivate = true)

    // Generate a field for each closured captured variable.
    for ((freeVar, index) <- closure.freeVars.zipWithIndex) {
      // `JvmType` of `freeVar`
      val varType = JvmOps.getErasedJvmType(freeVar.tpe)

      // `clo$index` field
      AsmOps.compileField(visitor, s"clo$index", varType, isStatic = false, isPrivate = true)
    }

    // Adding a setter and a field for each argument of the function
    for ((arg, index) <- targs.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedJvmType(arg)

      // `arg$index` field
      AsmOps.compileField(visitor, s"arg$index", argType, isStatic = false, isPrivate = true)

      // `setArg$index()` method
      AsmOps.compileSetFieldMethod(visitor, classType.name, s"arg$index", s"setArg$index", argType)
    }

    // Jvm type of the result of the function
    val resultType = JvmOps.getErasedJvmType(tresult)

    // Field for the result
    AsmOps.compileField(visitor, "result", resultType, isStatic = false, isPrivate = true)

    // Getter for the result field
    AsmOps.compileGetFieldMethod(visitor, classType.name, "result", "getResult", resultType)

    // Invoke method of the class
    compileInvokeMethod(visitor, classType, root.defs(closure.sym), closure.freeVars, resultType)

    // run method of the class
    compileRunMethod(visitor, root.defs(closure.sym), resultType)

    // Execute method of the class.
    compileApplyMethod(visitor, classType, root.defs(closure.sym), targs, tresult)

    // Constructor of the class
    compileConstructor(visitor, classType, closure.freeVars)

    visitor.toByteArray
  }

  /**
    * Apply method for the given `defn` and `classType`.
    */
  private def compileInvokeMethod(visitor: ClassWriter, classType: JvmType.Reference, defn: Def,
                                  freeVars: List[FreeVar], resultType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val invokeMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "invoke",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), null, null)

    // Free variables
    val frees = defn.formals.take(freeVars.length).map(x => FreeVar(x.sym, x.tpe))

    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - freeVars.length)

    // TODO Magnus, Jonathan, Simon: remove sanity checking
    // Sanity check
    val skipLabel = new Label
    invokeMethod.visitVarInsn(ALOAD, 0)
    //invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, "creationContext", JvmType.Object.toDescriptor)
    // line below was: invokeMethod.visitVarInsn(ALOAD, 1)
    invokeMethod.visitVarInsn(ALOAD, 0)

    // If contexts are equal, precede to evaluate
    invokeMethod.visitJumpInsn(IF_ACMPEQ, skipLabel)

    val message = "Closure is called with a different Context"
    // Create a new `Exception` object
    invokeMethod.visitTypeInsn(NEW, JvmName.Exception.toInternalName)
    invokeMethod.visitInsn(DUP)

    // add the message to the stack
    invokeMethod.visitLdcInsn(message)

    // invoke the constructor of the `Exception` object
    invokeMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Exception.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

    // throw the exception
    invokeMethod.visitInsn(ATHROW)

    // Visit skip label
    invokeMethod.visitLabel(skipLabel)

    // Enter label
    val enterLabel = new Label()
    invokeMethod.visitCode()

    // Saving free variables on variable stack
    for ((FreeVar(sym, tpe), ind) <- frees.zipWithIndex) {
      // Erased type of the free variable
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the free variable from IFO
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"clo$ind", erasedType.toDescriptor)

      // Saving the free variable on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, sym.getStackOffset + 3)
    }

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- params.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from IFO
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Saving the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, sym.getStackOffset + 3)
    }

    // Generating the expression
    GenExpression.compileExpression(defn.exp, invokeMethod, classType, Map(), enterLabel)

    // Loading `this`
    invokeMethod.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    if (AsmOps.getStackSize(resultType) == 1) {
      invokeMethod.visitInsn(SWAP)
    } else {
      invokeMethod.visitInsn(DUP_X2)
      invokeMethod.visitInsn(POP)
    }

    // Saving the result on the `result` field of IFO
    invokeMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "result", resultType.toDescriptor)

    // Return
    invokeMethod.visitInsn(RETURN)
    invokeMethod.visitMaxs(1, 1)
    invokeMethod.visitEnd()
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(visitor: ClassWriter, classType: JvmType.Reference, freeVars: List[FreeVar])(implicit root: Root, flix: Flix): Unit = {
    val varTypes = freeVars.map(_.tpe).map(JvmOps.getErasedJvmType)

    // Constructor header
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(JvmType.Object +: varTypes, JvmType.Void), null, null)

    // Calling constructor of super
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Saving the context
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitVarInsn(ALOAD, 1)
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "creationContext", JvmType.Object.toDescriptor)

    // Setting up closure args
    var offset: Int = 2
    for ((tpe, index) <- varTypes.zipWithIndex) {
      constructor.visitVarInsn(ALOAD, 0)

      val load = AsmOps.getLoadInstruction(tpe)
      constructor.visitVarInsn(load, offset)

      constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, s"clo$index", tpe.toDescriptor)

      // Incrementing the offset
      offset += AsmOps.getStackSize(tpe)
    }

    constructor.visitInsn(RETURN)
    constructor.visitMaxs(1, 1)
    constructor.visitEnd()
  }

  /**
    * Run method for the given `defn` and `classType`.
    */
  private def compileRunMethod(visitor: ClassWriter, defn: Def, resultType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // todo cant this just call apply? or unwind through existing functions

    // Method header
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "run",
      AsmOps.getMethodDescriptor(List(), JvmType.Void), null, null)

    // Put this on stack
    mv.visitVarInsn(ALOAD, 0)

    // Create new Context
    mv.visitTypeInsn(NEW, JvmName.Context.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName, "<init>", "()V", false)
    mv.visitVarInsn(ASTORE, 1)

    // Label for the loop
    val loop = new Label

    // Type of the function
    val fnType = root.defs(defn.sym).tpe

    // Type of the continuation interface
    val cont = JvmOps.getContinuationInterfaceType(fnType)

    // Store this ifo to the continuation field.
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Begin of the loop
    mv.visitLabel(loop)

    // Getting `continuation` field on `Context`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Setting `continuation` field of global to `null`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitInsn(ACONST_NULL)
    mv.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Cast to the continuation
    mv.visitTypeInsn(CHECKCAST, cont.name.toInternalName)

    // Duplicate
    mv.visitInsn(DUP)

    // Save it on the IFO local variable
    mv.visitVarInsn(ASTORE, 2)

    // Call invoke
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "invoke", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)

    // Getting `continuation` field on `Context`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    mv.visitJumpInsn(IFNONNULL, loop)

    // Load IFO from local variable and invoke `getResult` on it
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, resultType), true)
    AsmOps.boxIfPrim(mv, resultType)

    mv.visitInsn(RETURN)
    mv.visitMaxs(65535, 65535)
    mv.visitEnd()
  }


  /**
    * Spawn method for the given `defn` and `classType`.
    */
  private def compileApplyMethod(visitor: ClassWriter,
                                 classType: JvmType.Reference,
                                 defn: Def, targs: List[MonoType], resultType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    // The JVM result type
    val jvmResultType = JvmOps.getErasedJvmType(resultType)

    // Method header
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "apply",
      AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Object), null, null)

    // Iterate through each formal argument and invoke `setArg`.
    for ((tpe, index) <- targs.zipWithIndex) {
      // Load the `this` value (to be used for the call below).
      mv.visitVarInsn(ALOAD, 0)

      // Load the array.
      mv.visitVarInsn(ALOAD, 1)

      // Cast to an array.
      mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;")

      // Push the array index.
      mv.visitIntInsn(BIPUSH, index)

      // Load the element at the index.
      mv.visitInsn(AALOAD)

      // Invoke the setArgX method on `this`.
      val argErasedType = JvmOps.getErasedJvmType(tpe)

      // Cast and unbox.
      AsmOps.castIfNotPrimAndUnbox(argErasedType, mv)

      // Invoke setArgX.
      mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, s"setArg$index", AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), false)
    }

    // Put this on stack
    mv.visitVarInsn(ALOAD, 0)

    // Create new Context
    mv.visitTypeInsn(NEW, JvmName.Context.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName, "<init>", "()V", false)
    mv.visitVarInsn(ASTORE, 1)

    // Label for the loop
    val loop = new Label

    // Type of the function
    val fnType = root.defs(defn.sym).tpe

    // Type of the continuation interface
    val cont = JvmOps.getContinuationInterfaceType(fnType)

    // Store this ifo to the continuation field.
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Begin of the loop
    mv.visitLabel(loop)

    // Getting `continuation` field on `Context`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Setting `continuation` field of global to `null`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitInsn(ACONST_NULL)
    mv.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Cast to the continuation
    mv.visitTypeInsn(CHECKCAST, cont.name.toInternalName)

    // Duplicate
    mv.visitInsn(DUP)

    // Save it on the IFO local variable
    mv.visitVarInsn(ASTORE, 2)

    // Call invoke
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "invoke", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)

    // Getting `continuation` field on `Context`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    mv.visitJumpInsn(IFNONNULL, loop)

    // Load IFO from local variable and invoke `getResult` on it
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, jvmResultType), true)
    AsmOps.boxIfPrim(mv, jvmResultType)

    // Construct a proxy object.
    AsmOps.newProxyObject(resultType, mv)

    // Return the proxy object.
    mv.visitInsn(ARETURN)

    mv.visitMaxs(65535, 65535)
    mv.visitEnd()
  }

}
