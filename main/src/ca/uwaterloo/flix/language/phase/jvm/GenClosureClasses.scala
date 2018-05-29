package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, FormalParam, FreeVar, Root}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

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
    closures.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, closure) =>
        val jvmType = JvmOps.getClosureClassType(closure)
        val jvmName = jvmType.name
        val bytecode = genByteCode(closure)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }
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
    val args = closure.tpe.typeArguments

    // `JvmType` of the interface for `closure.tpe`
    val functionInterface = JvmOps.getFunctionInterfaceType(closure.tpe)

    // The super interface.
    val superInterface = Array(functionInterface.name.toInternalName)

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
    for ((arg, index) <- args.init.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedJvmType(arg)

      // `arg$index` field
      AsmOps.compileField(visitor, s"arg$index", argType, isStatic = false, isPrivate = true)

      // `setArg$index()` method
      AsmOps.compileSetFieldMethod(visitor, classType.name, s"arg$index", s"setArg$index", argType)
    }

    // Jvm type of the result of the function
    val resultType = JvmOps.getErasedJvmType(args.last)

    // Field for the result
    AsmOps.compileField(visitor, "result", resultType, isStatic = false, isPrivate = true)

    // Getter for the result field
    AsmOps.compileGetFieldMethod(visitor, classType.name, "result", "getResult", resultType)

    // Apply method of the class
    compileApplyMethod(visitor, classType, root.defs(closure.sym), closure.freeVars, resultType)

    // Copy method of the class
    compileCopyMethod(visitor, classType, functionInterface, root.defs(closure.sym), closure.freeVars, resultType)

    // Constructor of the class
    compileConstructor(visitor, classType, closure.freeVars)

    visitor.toByteArray
  }

  /**
    * copy method for the given `defn` and `classType`.
    *
    * public final `classType` copy(Context context) {
    *     `classType` closure = new `classType`(context, this.clo0, this.clo1, .., this.clon);
    *
    *     closure.setArg0(this.arg0);
    *     closure.setArg1(this.arg1);
    *     ...
    *     closure.setArgn(this.argn);
    *
    *     return closure;
    * }
    */
  private def compileCopyMethod(visitor: ClassWriter, classType: JvmType.Reference, functionInterface: JvmType.Reference, defn: Def,
                                 freeVars: List[FreeVar], resultType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // Variable types
    val varTypes = freeVars.map(_.tpe).map(JvmOps.getErasedJvmType)
    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - freeVars.length)

    // Method header
    val copyMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "copy",
      AsmOps.getMethodDescriptor(List(JvmType.Context), functionInterface), null, null)

    copyMethod.visitCode()

    copyMethod.visitTypeInsn(NEW, classType.name.toInternalName)
    copyMethod.visitInsn(DUP)
    copyMethod.visitVarInsn(ALOAD, 1)

    for ((tpe, index) <- varTypes.zipWithIndex) {
      copyMethod.visitVarInsn(ALOAD, 0)

      copyMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"clo$index", tpe.toDescriptor)
    }

    copyMethod.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(JvmType.Object +: varTypes, JvmType.Void), false)

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- params.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)
      copyMethod.visitInsn(DUP)

      copyMethod.visitVarInsn(ALOAD, 0)
      copyMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      copyMethod.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, s"setArg$ind",
        AsmOps.getMethodDescriptor(List(erasedType), JvmType.Void), false)
    }

    copyMethod.visitInsn(ARETURN)
    copyMethod.visitMaxs(1, 1)
    copyMethod.visitEnd()
  }

  /**
    * Apply method for the given `defn` and `classType`.
    */
  private def compileApplyMethod(visitor: ClassWriter, classType: JvmType.Reference, defn: Def,
                                 freeVars: List[FreeVar], resultType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val applyMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "apply",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), null, null)

    // Free variables
    val frees = defn.formals.take(freeVars.length).map(x => FreeVar(x.sym, x.tpe))

    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - freeVars.length)

    // Sanity check
    val skipLabel = new Label
    applyMethod.visitVarInsn(ALOAD, 0)
    applyMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, "creationContext", JvmType.Object.toDescriptor)
    applyMethod.visitVarInsn(ALOAD, 1)

    // If contexts are equal, precede to evaluate
    applyMethod.visitJumpInsn(IF_ACMPEQ, skipLabel)

    val message = "Closure is called with a different Context"
    // Create a new `Exception` object
    applyMethod.visitTypeInsn(NEW, JvmName.Exception.toInternalName)
    applyMethod.visitInsn(DUP)

    // add the message to the stack
    applyMethod.visitLdcInsn(message)

    // invoke the constructor of the `Exception` object
    applyMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Exception.toInternalName, "<init>",
    AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

    // throw the exception
    applyMethod.visitInsn(ATHROW)

    // Visit skip label
    applyMethod.visitLabel(skipLabel)

    // Enter label
    val enterLabel = new Label()
    applyMethod.visitCode()

    // Saving free variables on variable stack
    for ((FreeVar(sym, tpe), ind) <- frees.zipWithIndex) {
      // Erased type of the free variable
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the free variable from IFO
      applyMethod.visitVarInsn(ALOAD, 0)
      applyMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"clo$ind", erasedType.toDescriptor)

      // Saving the free variable on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      applyMethod.visitVarInsn(iSTORE, sym.getStackOffset + 3)
    }

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- params.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from IFO
      applyMethod.visitVarInsn(ALOAD, 0)
      applyMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Saving the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      applyMethod.visitVarInsn(iSTORE, sym.getStackOffset + 3)
    }

    // Generating the expression
    GenExpression.compileExpression(defn.exp, applyMethod, classType, Map(), enterLabel)

    // Loading `this`
    applyMethod.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    if (AsmOps.getStackSize(resultType) == 1) {
      applyMethod.visitInsn(SWAP)
    } else {
      applyMethod.visitInsn(DUP_X2)
      applyMethod.visitInsn(POP)
    }

    // Saving the result on the `result` field of IFO
    applyMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "result", resultType.toDescriptor)

    // Return
    applyMethod.visitInsn(RETURN)
    applyMethod.visitMaxs(1, 1)
    applyMethod.visitEnd()
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

}
