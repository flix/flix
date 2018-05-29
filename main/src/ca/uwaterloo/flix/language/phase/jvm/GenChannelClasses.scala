package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the channel classes.
  */
object GenChannelClasses {
  /**
    * Returns the bytecode for the channel classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // Type that we need a channel class for
    val types = List(JvmType.PrimBool, JvmType.PrimChar, JvmType.PrimFloat, JvmType.PrimDouble,
      JvmType.PrimByte, JvmType.PrimShort, JvmType.PrimInt, JvmType.PrimLong, JvmType.Object)

    // Generating each channel class
    types.map{ tpe =>
      val classType = JvmName.getChannelClassType(tpe)
      classType.name -> JvmClass(classType.name, genChannelClass(classType, tpe))
    }.toMap
  }

  /**
    * Generating class `classType` with value of type `channelType`
    */
  def genChannelClass(classType: JvmType.Reference, channelType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, null)

    // Generate the `queue` field
    AsmOps.compileField(visitor, "queue", JvmType.LinkedList, isStatic = false, isPrivate = true)

    // Generate the `lock` field
    AsmOps.compileField(visitor, "lock", JvmType.Lock, isStatic = false, isPrivate = true)

    // Generate the `capacity` field
    AsmOps.compileField(visitor, "capacity", JvmType.PrimInt, isStatic = false, isPrivate = true)

    // Generate the `channelGetters` field
    AsmOps.compileField(visitor, "channelGetters", JvmType.Condition, isStatic = false, isPrivate = true)

    // Generate the `channelPutters` field
    AsmOps.compileField(visitor, "channelPutters", JvmType.Condition, isStatic = false, isPrivate = true)

    // Generate the `selects` field
    AsmOps.compileField(visitor, "selects", JvmType.JavaList, isStatic = false, isPrivate = true)

    // Generate the constructor
    genConstructor(classType, channelType, visitor)

    // Generate `getValue` method
    genGetValue(classType, channelType, visitor)

    // Generate `putValue` method
    genPutValue(classType, channelType, visitor)

    // Generate `poll` method
    genPoll(classType, channelType, visitor)

    // Generate `offer` method
    genOffer(classType, channelType, visitor)

    // Generate `isEmpty` method
    genIsEmpty(classType, visitor)

    // Generate `isNonEmpty` method
    genIsNonEmpty(classType, visitor)

    // Generate `isFull` method
    genIsFull(classType, visitor)

    // Generate `size` method
    genSize(classType, visitor)

    // Generate `lock` method
    genLock(classType, visitor)

    // Generate `unlock` method
    genUnlock(classType, visitor)

    // Generate `signalGetters` method
    genSignalGetters(classType, visitor)

    // Generate `signalPutters` method
    genSignalPutters(classType, visitor)

    // Generate `clearSelects` method
    genClearSelects(classType, visitor)

    // Generate `awaitPutters` method
    genAwaitPutters(classType, visitor)

    // Generate `awaitGetters` method
    genAwaitGetters(classType, visitor)

    // Generate the `toString` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate the `hashCode` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate the `equals` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool),
      "equals method shouldn't be called")

    // Complete the visitor and get the bytecode.
    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generates the constructor of the channel `classType`
    */
  def genConstructor(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(JvmType.PrimInt), JvmType.Void), null, null)
    mv.visitCode()

    // Init the class object
    mv.visitVarInsn(ALOAD, 0)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Init the `queue` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitTypeInsn(NEW, JvmType.LinkedList.name.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmType.LinkedList.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)

    // Init the `lock` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitTypeInsn(NEW, JvmType.ReentrantLock.name.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmType.ReentrantLock.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)

    // Init the `selects` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitTypeInsn(NEW, JvmType.ArrayList.name.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmType.ArrayList.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "selects", JvmType.JavaList.toDescriptor)

    // Init `capacity` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ILOAD, 1)
    mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "capacity", JvmType.PrimInt.toDescriptor)

    // Init the `channelGetters` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "newCondition", AsmOps.getMethodDescriptor(Nil, JvmType.Condition), true)
    mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "channelGetters", JvmType.Condition.toDescriptor)

    // Init the `channelPutters` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "newCondition", AsmOps.getMethodDescriptor(Nil, JvmType.Condition), true)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitInsn(SWAP)
    mv.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "channelPutters", JvmType.Condition.toDescriptor)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(4, 2)
    mv.visitEnd()
  }

  /**
    * Generates the `getValue()` method of the `classType` with value of type `channelType`
    *
    * public `tpe` getValue() throws InterruptedException {
    *     `tpe` value = null;
    *     this.lock();
    *
    *     try {
    *         while(this.isEmpty()) {
    *             this.awaitPutters();
    *         }
    *
    *         value = this.poll();
    *         if (value != null) {
    *             this.signalPutters();
    *         }
    *     } finally {
    *         this.unlock();
    *     }
    *
    *     return value;
    * }
   */

  def genGetValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iRet = AsmOps.getReturnInstruction(channelType)
    val mv = visitor.visitMethod(ACC_PUBLIC, "getValue", AsmOps.getMethodDescriptor(Nil, channelType), null, Array(JvmName.InterruptedException.toInternalName))
    val loopStart = new Label()
    val loopEnd = new Label()
    val ifNullFalse = new Label()
    val labelStart = new Label()
    val labelEnd = new Label()
    val labelHandler = new Label()
    val labelEndHandler = new Label()

    mv.visitCode()

    // Integer = Null
    mv.visitInsn(ACONST_NULL)

    mv.visitTryCatchBlock(labelStart, labelEnd, labelHandler, null)

    // Lock()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Try
    mv.visitLabel(labelStart)

    // Loop
    mv.visitLabel(loopStart)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), false)

    mv.visitJumpInsn(IFEQ, loopEnd)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "awaitPutters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitJumpInsn(GOTO, loopStart)
    mv.visitLabel(loopEnd)

    //Integer = java.lang.Integer.valueOf(poll())
    mv.visitInsn(POP)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "poll", AsmOps.getMethodDescriptor(Nil, JvmType.Object), false)
    mv.visitTypeInsn(CHECKCAST, channelType.getBoxedTypeString)

    // if (Integer != null)
    mv.visitInsn(DUP)
    mv.visitJumpInsn(IFNULL, ifNullFalse)

    // signalPutters
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "signalPutters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitLabel(labelEnd)

    // Finally block - no exception
    mv.visitLabel(ifNullFalse)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitJumpInsn(GOTO, labelEndHandler)

    // Catch block
    mv.visitLabel(labelHandler)
    mv.visitVarInsn(ASTORE, 4)

    // Finally block - after catch block
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitVarInsn(ALOAD, 4)
    mv.visitInsn(ATHROW)
    mv.visitJumpInsn(GOTO, labelEndHandler)

    // Finally block - after exception not caught in catch block
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Return the value
    mv.visitLabel(labelEndHandler)
    if (channelType != JvmType.Object) {
      mv.visitMethodInsn(INVOKEVIRTUAL, channelType.getBoxedTypeString, channelType.getUnboxingMethod, AsmOps.getMethodDescriptor(Nil, channelType), false)
    }
    mv.visitInsn(iRet)
    mv.visitMaxs(4, 4)
    mv.visitEnd()
  }

  /**
    * Generates the `putValue()` method of the `classType` with value of type `channelType`
    *
    * public `classType` putValue(tpe value) throws InterruptedException {
    *     this.lock.lock();
    *
    *     try {
    *         while(this.isFull()) {
    *             this.awaitGetters();
    *         }
    *
    *         this.offer(value);
    *         this.signalGetters();
    *     } finally {
    *         this.lock.unlock();
    *     }
    *
    *     return this;
    * }
    */
  def genPutValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(channelType)
    val mv = visitor.visitMethod(ACC_PUBLIC, "putValue", AsmOps.getMethodDescriptor(List(channelType), classType), null, Array(JvmName.InterruptedException.toInternalName))
    val labelStart = new Label()
    val labelEnd = new Label()
    val labelHandler = new Label()
    val loopStart = new Label()
    val loopEnd = new Label()
    val labelReturn = new Label()
    mv.visitCode()

    // Lock
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    mv.visitTryCatchBlock(labelStart, labelEnd, labelHandler, null)

    // Try
    mv.visitLabel(labelStart)

    // Loop
    mv.visitLabel(loopStart)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "isFull", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), false)
    mv.visitJumpInsn(IFEQ, loopEnd)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "awaitGetters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    mv.visitJumpInsn(GOTO, loopStart)
    mv.visitLabel(loopEnd)

    // Offer
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(iLoad, 1)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "offer", AsmOps.getMethodDescriptor(List(channelType), JvmType.PrimBool), false)

    // TODO: Use the variable instead of just popping it
    mv.visitInsn(POP)

    // Signal All
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "signalGetters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // TODO: Clear Selects
    //mv.visitVarInsn(ALOAD, 0)
    //mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "clearSelects", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    mv.visitLabel(labelEnd)

    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    mv.visitJumpInsn(GOTO, labelReturn)

    // Catch
    mv.visitLabel(labelHandler)
    mv.visitInsn(POP)

    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitLabel(labelReturn)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitInsn(ARETURN)
    mv.visitMaxs(4, 4)
    mv.visitEnd()
  }

  /**
    * Generates the `poll()` method of the `classType`
    *
    * public Object poll() {
    *     return this.queue.poll();
    * }
    */
  def genPoll(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "poll", AsmOps.getMethodDescriptor(Nil, JvmType.Object), null, null)
    mv.visitCode()

    // Get the `queue` field & invoke the method `poll`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmType.LinkedList.name.toInternalName, "poll", AsmOps.getMethodDescriptor(Nil, JvmType.Object), false)

    // Return
    mv.visitInsn(ARETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `offer()` method of the `classType` with value of type `channelType`
    *
    * public boolean offer(tpe var1) {
    *     return this.queue.offer(var1);
    * }
    */
  def genOffer(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(channelType)
    val mv = visitor.visitMethod(ACC_PUBLIC, "offer", AsmOps.getMethodDescriptor(List(channelType), JvmType.PrimBool), null, null)
    mv.visitCode()

    // Get the `queue` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)

    // Load the parameter
    mv.visitVarInsn(iLoad, 1)

    // Don't use valueOf if the channelType is JvmType.Object
    if (channelType != JvmType.Object) {
      mv.visitMethodInsn(INVOKESTATIC, channelType.getBoxedTypeString, "valueOf", AsmOps.getMethodDescriptor(List(channelType), channelType.getBoxedType), false)
    }

    // Invoke the method `offer` on the `queue` field
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmType.LinkedList.name.toInternalName, "offer", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)

    // Return
    mv.visitInsn(IRETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `isEmpty()` method of the `classType`
    *
    * public boolean isEmpty() {
    *     return this.queue.isEmpty();
    * }
    */
  def genIsEmpty(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    mv.visitCode()

    // Get the `queue` field & invoke the method `isEmpty`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmType.LinkedList.name.toInternalName, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), false)

    // Return
    mv.visitInsn(IRETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `isNonEmpty()` method of the `classType`
    *
    * public boolean isNonEmpty() {
    *     return !this.queue.isEmpty();
    * }
    */
  def genIsNonEmpty(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "isNonEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    val labelElse = new Label()
    val labelEnd = new Label()
    mv.visitCode()

    // Get the `queue` field & invoke the method `isEmpty`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.LinkedList.name.toInternalName, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), true)

    // Check if the method `isEmpty` is true or false
    mv.visitJumpInsn(IFNE, labelElse)

    // The queue is not empty
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, labelEnd)

    // The queue is empty
    mv.visitLabel(labelElse)
    mv.visitInsn(ICONST_0)

    // Return
    mv.visitLabel(labelEnd)
    mv.visitInsn(IRETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `isFull()` method of the `classType`
    *
    * public boolean isFull() {
    *     return this.size() == this.capacity;
    * }
    */
  def genIsFull(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "isFull", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    val labelElse = new Label()
    val labelEnd = new Label()
    mv.visitCode()

    // Invoke the method `size` on the channel class of `channelType`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), false)

    // Get the `capacity` field
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "capacity", JvmType.PrimInt.toDescriptor)

    // Compare the size of the queue with the capacity
    mv.visitJumpInsn(IF_ICMPNE, labelElse)

    // The size of the queue and the capacity are equal
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, labelEnd)

    // The size of the queue and the capacity are not equal
    mv.visitLabel(labelElse)
    mv.visitInsn(ICONST_0)

    // Return
    mv.visitLabel(labelEnd)
    mv.visitInsn(IRETURN)
    mv.visitMaxs(2, 2)
    mv.visitEnd()
  }

  /**
    * Generates the `size()` method of the `classType`
    *
    * public int size() {
    *     return this.queue.size();
    * }
    */
  def genSize(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), null, null)
    mv.visitCode()

    // Get the `queue` field & invoke the method `size`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    mv.visitMethodInsn(INVOKEVIRTUAL, JvmType.LinkedList.name.toInternalName, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), false)

    // Return
    mv.visitInsn(IRETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `lock()` method of the `classType`
    *
    * public void lock() {
    *     this.lock.lock();
    * }
    */
  def genLock(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    mv.visitCode()

    // Get the `lock` field & invoke the method `lock`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `unlock()` method of the `classType`
    *
    * public void unlock() {
    *     this.lock.unlock();
    * }
    */
  def genUnlock(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    mv.visitCode()

    // Get the `lock` field & invoke the method `lock`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `signalGetters()` method of the `classType`
    *
    * public void signalGetters() {
    *     this.channelGetters.signalAll();
    * }
    */
  def genSignalGetters(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "signalGetters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    mv.visitCode()

    // Get the `channelGetters` field & invoke the method `signalAll`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelGetters", JvmType.Condition.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "signalAll", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `signalPutters()` method of the `classType`
    *
    * public void signalPutters() {
    *     this.channelPutters.signalAll();
    * }
    */
  def genSignalPutters(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "signalPutters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    mv.visitCode()

    // Get the `channelPutters` field & invoke the method `signalAll`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelPutters", JvmType.Condition.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "signalAll", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `clearSelects()` method of the `classType`
    *
    * public void clearSelects() {
    *     this.selects.clear();
    * }
    */
  def genClearSelects(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "clearSelects", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    mv.visitCode()

    // Get the `selects` field & invoke the method `clear`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "selects", JvmType.JavaList.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.JavaList.name.toInternalName, "clear", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `awaitPutters()` method of the `classType`
    *
    * public void awaitPutters() throws InterruptedException {
    *     this.channelGetters.await();
    * }
    */
  def genAwaitPutters(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "awaitPutters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, Array(JvmName.InterruptedException.toInternalName))
    mv.visitCode()

    // Get the `channelGetters` field & invoke the method `await`
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelGetters", JvmType.Condition.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "await", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  /**
    * Generates the `awaitGetters()` method of the `classType`
    *
    * public void awaitGetters() throws InterruptedException {
    *     this.channelPutters.await();
    * }
    */
  def genAwaitGetters(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, "awaitGetters", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, Array(JvmName.InterruptedException.toInternalName))
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)

    // Get the `channelPutters` field & invoke the method `await`
    mv.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelPutters", JvmType.Condition.toDescriptor)
    mv.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "await", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)

    // Return
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }
}
