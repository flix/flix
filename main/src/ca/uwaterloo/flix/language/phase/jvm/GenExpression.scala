/*
 * Copyright 2017 Ramin Zarifi
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

package ca.uwaterloo.flix.language.phase.jvm

import java.lang.reflect.Modifier

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.SemanticOperator._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.{InternalCompilerException, Optimization}
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._

/**
  * Generate expression
  */
object GenExpression {

  /**
    * Emits code for the given expression `exp0` to the given method `visitor` in the `currentClass`.
    */
  def compileExpression(exp0: Expression, visitor: MethodVisitor, currentClass: JvmType.Reference, lenv0: Map[Symbol.LabelSym, Label], entryPoint: Label)(implicit root: Root, flix: Flix): Unit = exp0 match {
    case Expression.Unit =>
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
        AsmOps.getMethodDescriptor(Nil, JvmType.Unit), false)
    case Expression.True => visitor.visitInsn(ICONST_1)
    case Expression.False => visitor.visitInsn(ICONST_0)
    case Expression.Char(c) => compileInt(visitor, c)
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
    case Expression.Int8(b) => compileInt(visitor, b)
    case Expression.Int16(s) => compileInt(visitor, s)
    case Expression.Int32(i) => compileInt(visitor, i)
    case Expression.Int64(l) => compileInt(visitor, l, isLong = true)
    case Expression.BigInt(ii) =>
      visitor.visitTypeInsn(NEW, JvmName.BigInteger.toInternalName)
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(ii.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, JvmName.BigInteger.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
    case Expression.Str(s) => visitor.visitLdcInsn(s)

    case Expression.Var(sym, tpe, _) =>
      val jvmType = JvmOps.getErasedJvmType(tpe)
      val iLOAD = AsmOps.getLoadInstruction(jvmType)
      visitor.visitVarInsn(iLOAD, sym.getStackOffset + 3) // This is `+2` because the first 2 are reserved!
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
      // ClosureInfo
      val closure = ClosureInfo(sym, freeVars, fnType)
      // JvmType of the closure
      val jvmType = JvmOps.getClosureClassType(closure)
      // new closure instance
      visitor.visitTypeInsn(NEW, jvmType.name.toInternalName)
      // Duplicate
      visitor.visitInsn(DUP)
      // Load the context object
      visitor.visitVarInsn(ALOAD, 1)
      // Capturing free args
      for (f <- freeVars) {
        val v = Expression.Var(f.sym, f.tpe, loc)
        compileExpression(v, visitor, currentClass, lenv0, entryPoint)
      }
      // Calling the constructor
      val varTypes = freeVars.map(_.tpe).map(JvmOps.getErasedJvmType)
      visitor.visitMethodInsn(INVOKESPECIAL, jvmType.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(JvmType.Object +: varTypes, JvmType.Void), false)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      // Label for the loop
      val loop = new Label
      // Type of the continuation interface
      val cont = JvmOps.getContinuationInterfaceType(exp.tpe)
      // Type of the function interface
      val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
      // Result type
      val resultType = JvmOps.getErasedJvmType(tpe)
      // Put the closure on `continuation` field of `Context`
      visitor.visitVarInsn(ALOAD, 1)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Casting to JvmType of FunctionInterface
      visitor.visitTypeInsn(CHECKCAST, functionInterface.name.toInternalName)
      // Saving the continuation so we don't have to use calculate this again
      visitor.visitInsn(DUP)
      // Putting args on the stack
      for (arg <- args) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Erased Type
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        if (AsmOps.getStackSize(argErasedType) == 1) {
          visitor.visitInsn(SWAP)
        } else {
          visitor.visitInsn(DUP2_X1)
          visitor.visitInsn(POP2)
        }
      }
      visitor.visitInsn(POP)
      // Saving args on the continuation interface in reverse
      for ((arg, ind) <- args.zipWithIndex.reverse) {
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        visitor.visitMethodInsn(INVOKEINTERFACE, functionInterface.name.toInternalName, s"setArg$ind",
          AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), true)
      }
      visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Begin of the loop
      visitor.visitLabel(loop)
      // Getting `continuation` field on `Context`
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Setting `continuation` field of global to `null`
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitInsn(ACONST_NULL)
      visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Cast to the continuation
      visitor.visitTypeInsn(CHECKCAST, cont.name.toInternalName)
      // Duplicate
      visitor.visitInsn(DUP)
      // Save it on the IFO local variable
      visitor.visitVarInsn(ASTORE, 2)
      // Call apply
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "apply", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)
      // Getting `continuation` field on `Context`
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      visitor.visitJumpInsn(IFNONNULL, loop)
      // Load IFO from local variable and invoke `getResult` on it
      visitor.visitVarInsn(ALOAD, 2)
      visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, resultType), true)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.ApplyDef(name, args, tpe, loc) =>
      // Label for the loop
      val loop = new Label
      // Namespace of the Def
      val ns = JvmOps.getNamespace(name)
      // JvmType of `ns`
      val nsJvmType = JvmOps.getNamespaceClassType(ns)
      // Name of the field for `ns` on `Context`
      val nsFieldName = JvmOps.getNamespaceFieldNameInContextClass(ns)
      // Field for Def on `ns`
      val defFiledName = JvmOps.getDefFieldNameInNamespaceClass(name)
      // JvmType of Def
      val defJvmType = JvmOps.getFunctionDefinitionClassType(name)
      // Type of the function
      val fnType = root.defs(name).tpe
      // Type of the continuation interface
      val cont = JvmOps.getContinuationInterfaceType(fnType)
      // Type of the function interface
      val functionInterface = JvmOps.getFunctionInterfaceType(fnType)
      // Put the closure on `continuation` field of `Context`
      visitor.visitVarInsn(ALOAD, 1)
      // Load `Context`
      visitor.visitVarInsn(ALOAD, 1)
      // Load `ns`
      visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, nsFieldName, nsJvmType.toDescriptor)
      // Load `continuation`
      visitor.visitFieldInsn(GETFIELD, nsJvmType.name.toInternalName, defFiledName, defJvmType.toDescriptor)
      // Result type
      val resultType = JvmOps.getErasedJvmType(tpe)
      // Casting to JvmType of FunctionInterface
      visitor.visitTypeInsn(CHECKCAST, functionInterface.name.toInternalName)
      visitor.visitInsn(DUP)
      // Putting args on the stack
      for (arg <- args) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Erased Type
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        if (AsmOps.getStackSize(argErasedType) == 1) {
          visitor.visitInsn(SWAP)
        } else {
          visitor.visitInsn(DUP2_X1)
          visitor.visitInsn(POP2)
        }
      }
      visitor.visitInsn(POP)
      // Saving args on the continuation interface in reverse
      for ((arg, ind) <- args.zipWithIndex.reverse) {
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        visitor.visitMethodInsn(INVOKEINTERFACE, functionInterface.name.toInternalName, s"setArg$ind",
          AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), true)
      }
      visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Begin of the loop
      visitor.visitLabel(loop)
      // Getting `continuation` field on `Context`
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

      // Setting `continuation` field of global to `null`
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitInsn(ACONST_NULL)
      visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Cast to the continuation
      visitor.visitTypeInsn(CHECKCAST, cont.name.toInternalName)
      // Duplicate
      visitor.visitInsn(DUP)
      // Save it on the IFO local variable
      visitor.visitVarInsn(ASTORE, 2)
      // Call apply
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "apply", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)
      // Getting `continuation` field on `Context`
      visitor.visitVarInsn(ALOAD, 1)
      visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      visitor.visitJumpInsn(IFNONNULL, loop)
      // Load IFO from local variable and invoke `getResult` on it
      visitor.visitVarInsn(ALOAD, 2)
      visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, resultType), true)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.ApplyEff(sym, args, tpe, loc) =>
      throw InternalCompilerException(s"ApplyEff not implemented in JVM backend!")

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      // Type of the function interface
      val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
      // Result type
      val resultType = JvmOps.getErasedJvmType(tpe)
      // Loading `Context`
      visitor.visitVarInsn(ALOAD, 1)
      // Evaluating the closure
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Casting to JvmType of FunctionInterface
      visitor.visitTypeInsn(CHECKCAST, functionInterface.name.toInternalName)
      // Saving the continuation so we don't have to use calculate this again
      visitor.visitInsn(DUP)
      // Putting args on the stack
      for (arg <- args) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Erased Type
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        if (AsmOps.getStackSize(argErasedType) == 1) {
          visitor.visitInsn(SWAP)
        } else {
          visitor.visitInsn(DUP2_X1)
          visitor.visitInsn(POP2)
        }
      }
      visitor.visitInsn(POP)
      // Saving args to continuation in reverse order
      for ((arg, ind) <- args.zipWithIndex.reverse) {
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        // Setting the arg
        visitor.visitMethodInsn(INVOKEINTERFACE, functionInterface.name.toInternalName, s"setArg$ind",
          AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), true)
      }
      // Placing the interface on continuation field of `Context`
      visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Dummy value, since we have to put a result on top of the arg, this will be thrown away
      pushDummyValue(visitor, tpe)

    case Expression.ApplyDefTail(name, args, tpe, loc) =>
      // Namespace of the Def
      val ns = JvmOps.getNamespace(name)
      // JvmType of `ns`
      val nsJvmType = JvmOps.getNamespaceClassType(ns)
      // Name of the field for `ns` on `Context`
      val nsFieldName = JvmOps.getNamespaceFieldNameInContextClass(ns)
      // Field for Def on `ns`
      val defFiledName = JvmOps.getDefFieldNameInNamespaceClass(name)
      // JvmType of Def
      val defJvmType = JvmOps.getFunctionDefinitionClassType(name)
      // Type of the function
      val fnType = root.defs(name).tpe
      // Type of the continuation interface
      val cont = JvmOps.getContinuationInterfaceType(fnType)
      // Type of the function interface
      val functionInterface = JvmOps.getFunctionInterfaceType(fnType)
      // Put the def on `continuation` field of `Context`
      visitor.visitVarInsn(ALOAD, 1)
      // Load `Context`
      visitor.visitVarInsn(ALOAD, 1)
      // Load `ns`
      visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, nsFieldName, nsJvmType.toDescriptor)
      // Load Function
      visitor.visitFieldInsn(GETFIELD, nsJvmType.name.toInternalName, defFiledName, defJvmType.toDescriptor)
      // Result type
      val resultType = JvmOps.getErasedJvmType(tpe)
      // Casting to JvmType of FunctionInterface
      visitor.visitTypeInsn(CHECKCAST, functionInterface.name.toInternalName)
      // Putting args on the stack
      visitor.visitInsn(DUP)
      for (arg <- args) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Erased Type
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        if (AsmOps.getStackSize(argErasedType) == 1) {
          visitor.visitInsn(SWAP)
        } else {
          visitor.visitInsn(DUP2_X1)
          visitor.visitInsn(POP2)
        }
      }
      visitor.visitInsn(POP)
      // Saving args on the continuation in reverse order
      for ((arg, ind) <- args.zipWithIndex.reverse) {
        val argErasedType = JvmOps.getErasedJvmType(arg.tpe)
        visitor.visitMethodInsn(INVOKEINTERFACE, functionInterface.name.toInternalName, s"setArg$ind",
          AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), true)
      }
      // Placing the interface on continuation field of `Context`
      visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
      // Dummy value, since we have to put a result on top of the arg, this will be thrown away
      pushDummyValue(visitor, tpe)

    case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
      // Evaluate each argument and push the result on the stack.
      for (arg <- actuals) {
        visitor.visitVarInsn(ALOAD, 0)
        // Evaluate the argument and push the result on the stack.
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
      }
      // The values are on the stack in reverse order, so we must iterate over the arguments in reverse order.
      for ((arg, ind) <- actuals.zipWithIndex.reverse) {
        val argType = JvmOps.getErasedJvmType(arg.tpe)
        visitor.visitMethodInsn(INVOKEVIRTUAL, currentClass.name.toInternalName, s"setArg$ind",
          AsmOps.getMethodDescriptor(List(argType), JvmType.Void), false)
      }
      // Jump to the entry point of the method.
      visitor.visitJumpInsn(GOTO, entryPoint)

    case Expression.Unary(sop, op, exp, _, _) =>
      // TODO: Ramin: Must not use `op`, should only use `sop`.
      compileUnaryExpr(exp, currentClass, visitor, lenv0, entryPoint, op, sop)

    case Expression.Binary(sop, op, exp1, exp2, _, _) =>
      // TODO: Ramin: Must not use `op`, should only use `sop`.
      // TODO: Ramin: Probably better to group these methods by type, e.g. compileFloat32Exp. (See interpreter for a possible structure).
      op match {
        case o: ArithmeticOperator => compileArithmeticExpr(exp1, exp2, currentClass, visitor, lenv0, entryPoint, o, sop)
        case o: ComparisonOperator => compileComparisonExpr(exp1, exp2, currentClass, visitor, lenv0, entryPoint, o, sop)
        case o: LogicalOperator => compileLogicalExpr(exp1, exp2, currentClass, visitor, lenv0, entryPoint, o)
        case o: BitwiseOperator => compileBitwiseExpr(exp1, exp2, currentClass, visitor, lenv0, entryPoint, o, sop)
      }

    case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val ifElse = new Label()
      val ifEnd = new Label()
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      visitor.visitJumpInsn(IFEQ, ifElse)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
      visitor.visitJumpInsn(GOTO, ifEnd)
      visitor.visitLabel(ifElse)
      compileExpression(exp3, visitor, currentClass, lenv0, entryPoint)
      visitor.visitLabel(ifEnd)

    case Expression.Branch(exp, branches, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Calculating the updated jumpLabels map
      val updatedJumpLabels = branches.foldLeft(lenv0)((map, branch) => map + (branch._1 -> new Label()))
      // Compiling the exp
      compileExpression(exp, visitor, currentClass, updatedJumpLabels, entryPoint)
      // Label for the end of all branches
      val endLabel = new Label()
      // Skip branches if `exp` does not jump
      visitor.visitJumpInsn(GOTO, endLabel)
      // Compiling branches
      branches.foreach { case (sym, branchExp) =>
        // Label for the start of the branch
        visitor.visitLabel(updatedJumpLabels(sym))
        // evaluating the expression for the branch
        compileExpression(branchExp, visitor, currentClass, updatedJumpLabels, entryPoint)
        // Skip the rest of the branches
        visitor.visitJumpInsn(GOTO, endLabel)
      }
      // label for the end of branches
      visitor.visitLabel(endLabel)

    case Expression.JumpTo(sym, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Jumping to the label
      visitor.visitJumpInsn(GOTO, lenv0(sym))

    case Expression.Let(sym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      visitor.visitVarInsn(iStore, sym.getStackOffset + 3)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)

    case Expression.LetRec(sym, exp1, exp2, _, _) =>
      ??? // TODO: Ramin: Implement let rec. (The signature of let rec might need to change.)

    case Expression.Is(enum, tag, exp, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Case 1: Check for unwrappability.
      if (JvmOps.isSingleCaseEnum(enum)) {
        // TODO: Ramin: Add support for single case enums?
      }
      // Case 2: Check for nullability.
      if (JvmOps.isNullable(exp.tpe)) {
        // Compile the expression
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

        /*
         * If type of the expression is Unit, then use the `IFNONNULL` comparator which will jump to `falseLabel` if the
         * object is not null. If the type of expression is not Unit, then we use `IFNULL` comparator which will jump to
         * `falseLabel` if the object is null.
         */
        val comparator = if (JvmOps.isNullTag(enum, tag)) {
          IFNONNULL
        } else {
          IFNULL
        }
        // Label to the end of the comparison
        val endLabel = new Label()
        // Label to `false`
        val falseLabel = new Label()
        visitor.visitJumpInsn(comparator, falseLabel)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(GOTO, endLabel)
        visitor.visitLabel(falseLabel)
        visitor.visitInsn(ICONST_0)
        visitor.visitLabel(endLabel)
      }
      // Case 3: Ordinary enum.
      else {
        // We get the `TagInfo` for the tag
        val tagInfo = JvmOps.getTagInfo(exp.tpe, tag)
        // Fusion info
        val fusionInfo = JvmOps.getFusionTag(tagInfo)
        // We get the JvmType of the class for tag
        val classType = if (fusionInfo.isDefined && flix.options.optimizations.contains(Optimization.TagTupleFusion)) {
          JvmOps.getFusionClassType(fusionInfo.get)
        } else {
          JvmOps.getTagClassType(tagInfo)
        }
        // First we compile the `exp`
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // We check if the enum is `instanceof` the class
        visitor.visitTypeInsn(INSTANCEOF, classType.name.toInternalName)
      }

    // Fusion when we directly create a tuple
    case Expression.Tag(enum, tag, exp@Expression.Tuple(elms, _, _), tpe, loc) if flix.options.optimizations.contains(Optimization.TagTupleFusion) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(tpe, tag)
      // Fusion info
      val fusionInfo = JvmOps.getFusionTag(tagInfo).get
      // Jvm type of the class
      val classType = JvmOps.getFusionClassType(fusionInfo)
      // JvmType of `elms`
      val fieldTypes = elms.map(elm => JvmOps.getErasedJvmType(elm.tpe)).toList
      // Creating a new instance of the case
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicating the class
      visitor.visitInsn(DUP)
      // Evaluating all the elements to be stored in the tuple class
      elms.foreach {
        compileExpression(_, visitor, currentClass, lenv0, entryPoint)
      }
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(fieldTypes, JvmType.Void), false)

    // Fusion when the tuple is already created
    case Expression.Tag(enum, tag, exp, tpe, loc) if exp.tpe.isTuple && flix.options.optimizations.contains(Optimization.TagTupleFusion) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(tpe, tag)
      // Fusion info
      val fusionInfo = JvmOps.getFusionTag(tagInfo).get
      // Jvm type of the class
      val classType = JvmOps.getFusionClassType(fusionInfo)
      // Jvm type of the tuple
      val tupleType = JvmOps.getTupleInterfaceType(exp.tpe)
      // JvmType of arguments of tuple
      val fieldTypes = exp.tpe.typeArguments.map(JvmOps.getErasedJvmType)
      // Creating a new instance of the case
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicating the class
      visitor.visitInsn(DUP)
      // Evaluating the expression for the value of the tag
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Extracting all indices of the tuple
      for ((jvmType, ind) <- fieldTypes.zipWithIndex) {
        // Duplicating the reference since the function call will consume one reference
        visitor.visitInsn(DUP)
        // Extracting value of `index` index from the tuple
        visitor.visitMethodInsn(INVOKEINTERFACE, tupleType.name.toInternalName, s"getIndex$ind",
          AsmOps.getMethodDescriptor(Nil, jvmType), true)
        // Bringing the reference to the tuple to the top of the stack
        if (AsmOps.getStackSize(jvmType) == 1) {
          visitor.visitInsn(SWAP)
        }
        else {
          visitor.visitInsn(DUP2_X1)
          visitor.visitInsn(POP2)
        }
      }
      // Popping the reference to the tuple from the stack
      visitor.visitInsn(POP)
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(fieldTypes, JvmType.Void), false)

    // Normal Tag
    case Expression.Tag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)

      // Case 1: Check for unwrappability.
      if (JvmOps.isSingleCaseEnum(enum)) {
        // TODO: Ramin: Add support for single case enums?
      }
      // Case 2: Check for nullability.
      if (JvmOps.isNullable(tpe)) {
        if (JvmOps.isNullTag(enum, tag)) {
          // If the field is Unit, we push a NULL to the top of the stack
          visitor.visitInsn(ACONST_NULL)
        } else {
          /*
           * If the field type is Unit, we first evaluate the expression, then we pop the result from the top of the
           * stack and push a NULL to the top of the stack.
           */
          compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        }
      }
      // Case 3: Ordinary enum.
      else {
        val tagInfo = JvmOps.getTagInfo(tpe, tag)
        // We get the JvmType of the class for tag
        val classType = JvmOps.getTagClassType(tagInfo)
        /*
       If the definition of the enum case has a `Unit` field, then it is represented by singleton pattern which means
       there is only one instance of the class initiated as a field. We have to fetch this field instead of instantiating
       a new one.
       */
        if (JvmOps.isUnitTag(tagInfo)) {
          visitor.visitFieldInsn(GETSTATIC, classType.name.toInternalName, "unitInstance", classType.toDescriptor)
        } else {
          // Creating a new instance of the class
          visitor.visitTypeInsn(NEW, classType.name.toInternalName)
          visitor.visitInsn(DUP)
          // Evaluating the single argument of the class constructor
          compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
          // Descriptor of the constructor
          val constructorDescriptor = AsmOps.getMethodDescriptor(List(JvmOps.getErasedJvmType(tagInfo.tagType)), JvmType.Void)
          // Calling the constructor of the class
          visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)
        }
      }

    case Expression.Untag(enum, tag, exp, tpe, loc) if tpe.isTuple && flix.options.optimizations.contains(Optimization.TagTupleFusion) =>
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

    case Expression.Untag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Case 1: Check for unwrappability.
      if (JvmOps.isSingleCaseEnum(enum)) {
        // TODO: Ramin: Add support for single case enums?
      }
      // Case 2: Check for nullability.
      if (JvmOps.isNullable(exp.tpe)) {
        if (JvmOps.isNullTag(enum, tag)) {
          exp match {
            case Expression.Var(_, _, _) =>
              /*
               * If sub expression is a var, then that expression has already been evaluated. Since the result is just NULL,
               * we will not evaluate the sub expression and we will directly put a Unit on top of the stack.
               */
              visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
                AsmOps.getMethodDescriptor(Nil, JvmType.Unit), false)
            case _ =>
              /*
               * If the sub expression is of type Unit, we evaluate the expression.
               */
              compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
          }
        } else {
          // Else we just evaluate the expression
          compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        }
      } // Case 3: Ordinary enum.
      else {
        // We get the `TagInfo` for the tag
        val tagInfo = JvmOps.getTagInfo(exp.tpe, tag)
        // We get the JvmType of the class for the tag
        val classType = JvmOps.getTagClassType(tagInfo)
        // Evaluate the exp
        compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
        // Cast the exp to the type of the tag
        visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
        // Descriptor of the method
        val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tagInfo.tagType))
        // Invoke `getValue()` method to extract the field of the tag
        visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getValue", methodDescriptor, false)
        // Cast the object to it's type if it's not a primitive
        AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))
      }

    case Expression.Index(base, offset, tpe, _) =>
      // We get the JvmType of the class for the tuple
      val classType = JvmOps.getTupleInterfaceType(base.tpe)
      // evaluating the `base`
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Descriptor of the method
      val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe))
      // Invoking `getField${offset}()` method for fetching the field
      visitor.visitMethodInsn(INVOKEINTERFACE, classType.name.toInternalName, s"getIndex$offset", methodDescriptor, true)
      // Cast the object to it's type if it's not a primitive
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.Tuple(elms, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the JvmType of the class for the tuple
      val classType = JvmOps.getTupleClassType(tpe)
      // Instantiating a new object of tuple
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicating the class
      visitor.visitInsn(DUP)
      // Evaluating all the elements to be stored in the tuple class
      elms.foreach(compileExpression(_, visitor, currentClass, lenv0, entryPoint))
      // Erased type of `elms`
      val erasedElmTypes = elms.map(_.tpe).map(JvmOps.getErasedJvmType).toList
      // Descriptor of constructor
      val constructorDescriptor = AsmOps.getMethodDescriptor(erasedElmTypes, JvmType.Void)
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

    case Expression.ArrayLit(elms, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We push the 'length' of the array on top of stack
      compileInt(visitor, elms.length, isLong = false)
      // We get the inner type of the array
      val jvmType = JvmOps.getErasedJvmType(JvmOps.getArrayInnerType(tpe))
      // Instantiating a new array of type jvmType
      if(jvmType == JvmType.Object){ // Happens if the inner type is an object type
        visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
      }
      else{ // Happens if the inner type is a primitive type
        visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
      }
      // For each element we generate code to store it into the array
      for(i <- 0 until elms.length){
        // Duplicates the 'array reference'
        visitor.visitInsn(DUP)
        // We push the 'index' of the current element on top of stack
        compileInt(visitor, i, isLong = false)
        // Evaluating the 'element' to be stored
        compileExpression(elms(i), visitor, currentClass, lenv0, entryPoint)
        // Stores the 'element' at the given 'index' in the 'array'
        // with the store instruction corresponding to the stored element
        visitor.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
      }

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the inner type of the array
      val jvmType = JvmOps.getErasedJvmType(JvmOps.getArrayInnerType(tpe))
      // Evaluating the 'length' of the array
      compileExpression(len, visitor, currentClass, lenv0, entryPoint)
      // Instantiating a new array of type jvmType
      if(jvmType == JvmType.Object){ // Happens if the inner type is an object type
        visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
      }
      else{ // Happens if the inner type is a primitive type
        visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
      }
      // Duplicates the 'array reference'
      visitor.visitInsn(DUP)
      // Evaluating the value of the 'default element'
      compileExpression(elm, visitor, currentClass, lenv0, entryPoint)
      // We get the array fill type
      val arrayFillType = AsmOps.getArrayFillType(jvmType)
      // Invoking the method to fill the array with the default element
      visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Arrays", "fill", arrayFillType, false);

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the jvmType of the element to be loaded
      val jvmType = JvmOps.getErasedJvmType(tpe)
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Cast the object to Array
      visitor.visitTypeInsn(CHECKCAST, AsmOps.getCheckCastType(jvmType))
      // Evaluating the 'index' to load from
      compileExpression(index, visitor, currentClass, lenv0, entryPoint)
      // Loads the 'element' at the given 'index' from the 'array'
      // with the load instruction corresponding to the loaded element
      visitor.visitInsn(AsmOps.getArrayLoadInstruction(jvmType))

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the jvmType of the element to be stored
      val jvmType = JvmOps.getErasedJvmType(elm.tpe)
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Cast the object to Array
      visitor.visitTypeInsn(CHECKCAST, AsmOps.getCheckCastType(jvmType))
      // Evaluating the 'index' to be stored in
      compileExpression(index, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'element' to be stored
      compileExpression(elm, visitor, currentClass, lenv0, entryPoint)
      // Stores the 'element' at the given 'index' in the 'array'
      // with the store instruction corresponding to the stored element
      visitor.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
      // Since the return type is 'unit', we put an instance of 'unit' on top of the stack
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
                              AsmOps.getMethodDescriptor(Nil, JvmType.Unit), false)

    case Expression.ArrayLength(base, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the inner type of the array
      val jvmType = JvmOps.getErasedJvmType(JvmOps.getArrayInnerType(base.tpe))
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Cast the object to array
      visitor.visitTypeInsn(CHECKCAST, AsmOps.getCheckCastType(jvmType))
      // Pushes the 'length' of the array on top of stack
      visitor.visitInsn(ARRAYLENGTH)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the inner type of the array
      val jvmType = JvmOps.getErasedJvmType(JvmOps.getArrayInnerType(base.tpe))
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'beginIndex'
      compileExpression(beginIndex, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'endIndex'
      compileExpression(endIndex, visitor, currentClass, lenv0, entryPoint)
      // Swaps the beginIndex and 'endIndex'
      visitor.visitInsn(SWAP)
      // Duplicates the 'beginIndex' two places down the stack
      visitor.visitInsn(DUP_X1)
      // Subtracts the 'beginIndex' from the 'endIndex' (leaving the 'length' of the array)
      visitor.visitInsn(ISUB)
      // Duplicates the 'length'
      visitor.visitInsn(DUP)
      // Instantiating a new array of type jvmType
      if(jvmType == JvmType.Object){ // Happens if the inner type is an object type
        visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
      }
      else{ // Happens if the inner type is a primitive type
        visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
      }
      // Duplicates the 'array reference' and 'length' 4 places down the stack
      visitor.visitInsn(DUP2_X2)
      // Swaps the 'array reference' and 'length'
      visitor.visitInsn(SWAP)
      // Pushes 0 on top of stack
      visitor.visitInsn(ICONST_0)
      // Swaps 'length' and 0
      visitor.visitInsn(SWAP)
      // Invoking the method to copy the source array to the destination array
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false);
      // Swaps 'new array reference' and 'length'
      visitor.visitInsn(SWAP)
      // Pops the 'length' - leaving 'new array reference' top of stack
      visitor.visitInsn(POP)

    case Expression.Ref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // JvmType of the reference class
      val classType = JvmOps.getCellClassType(tpe)
      // Create a new reference object
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicate it since one instance will get consumed by constructor
      visitor.visitInsn(DUP)
      // Evaluate the underlying expression
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Erased type of the value of the reference
      val valueErasedType = JvmOps.getErasedJvmType(tpe.typeArguments.head)
      // Constructor descriptor
      val constructorDescriptor = AsmOps.getMethodDescriptor(List(valueErasedType), JvmType.Void)
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

    case Expression.Deref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the exp
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // JvmType of the reference class
      val classType = JvmOps.getCellClassType(exp.tpe)
      // Get descriptor of `getValue` method
      val methodDescriptor = AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe))
      // Dereference the expression
      visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getValue", methodDescriptor, false)
      // Cast underlying value to the correct type if the underlying type is Object
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the reference address
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the value to be assigned to the reference
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
      // JvmType of the reference class
      val classType = JvmOps.getCellClassType(exp1.tpe)
      // Get descriptor of `setValue` method
      val methodDescriptor = AsmOps.getMethodDescriptor(List(JvmOps.getErasedJvmType(exp2.tpe)), JvmType.Void)
      // Invoke `setValue` method to set the value to the given number
      visitor.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "setValue", methodDescriptor, false)
      // Since the return type is unit, we put an instance of unit on top of the stack
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
        AsmOps.getMethodDescriptor(Nil, JvmType.Unit), false)

    case Expression.Existential(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$exp0' at ${loc.source.format}.")

    case Expression.Universal(params, exp, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$exp0' at ${loc.source.format}.")

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
      args.foreach(compileExpression(_, visitor, currentClass, lenv0, entryPoint))
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
      visitor.visitFieldInsn(getInsn, declaration, name, JvmOps.getJvmType(tpe).toDescriptor)

    case Expression.NativeMethod(method, args, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate arguments left-to-right and push them onto the stack.
      args.foreach(compileExpression(_, visitor, currentClass, lenv0, entryPoint))
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)
      // If the method is static, use INVOKESTATIC otherwise use INVOKEVIRTUAL
      val invokeInsn = if (Modifier.isStatic(method.getModifiers)) INVOKESTATIC else INVOKEVIRTUAL
      visitor.visitMethodInsn(invokeInsn, declaration, name, descriptor, false)
      // If the method is void, put a unit on top of the stack
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
        visitor.visitMethodInsn(INVOKESTATIC, JvmName.Unit.toInternalName, "getInstance",
          AsmOps.getMethodDescriptor(List(), JvmType.Unit), false)
      }

    case Expression.UserError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val msg = s"User exception: ${loc.format}."
      AsmOps.compileThrowException(visitor, JvmName.UserException, msg)

    case Expression.HoleError(sym, _, loc) => ???
    // TODO: Ramin: HoleError.
    // TODO: Be sure to Uncomment the tests in Test.Expression.Hole.

    case Expression.MatchError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val msg = s"Non-exhaustive match expression: ${loc.format}."
      AsmOps.compileThrowException(visitor, JvmName.MatchException, msg)

    case Expression.SwitchError(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val msg = s"Non-exhaustive switch expression: ${loc.format}."
      AsmOps.compileThrowException(visitor, JvmName.SwitchException, msg)
  }

  /*
   * Pushes a dummy value of type `jvmType` to the top of the stack
   */
  private def pushDummyValue(visitor: MethodVisitor, tpe: ca.uwaterloo.flix.language.ast.Type)(implicit root: Root, flix: Flix): Unit = {
    val erasedType = JvmOps.getErasedJvmType(tpe)
    erasedType match {
      case JvmType.Void => throw InternalCompilerException(s"Unexpected type: $erasedType")
      case JvmType.PrimBool => visitor.visitInsn(ICONST_1)
      case JvmType.PrimChar => visitor.visitInsn(ICONST_M1)
      case JvmType.PrimByte => visitor.visitInsn(ICONST_M1)
      case JvmType.PrimShort => visitor.visitInsn(ICONST_M1)
      case JvmType.PrimInt => visitor.visitInsn(ICONST_M1)
      case JvmType.PrimLong =>
        visitor.visitInsn(ICONST_M1)
        visitor.visitInsn(I2L)
      case JvmType.PrimFloat => visitor.visitInsn(FCONST_1)
      case JvmType.PrimDouble => visitor.visitInsn(DCONST_1)
      case JvmType.Reference(_) => visitor.visitInsn(ACONST_NULL)
    }
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
  private def compileInt(visitor: MethodVisitor, i: Long, isLong: Boolean = false): Unit = {
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

  private def compileUnaryExpr(e: Expression,
                               currentClassType: JvmType.Reference,
                               visitor: MethodVisitor,
                               jumpLabels: Map[Symbol.LabelSym, Label],
                               entryPoint: Label,
                               op: UnaryOperator,
                               sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = {
    // Adding source line number for debugging
    addSourceLine(visitor, e.loc)

    compileExpression(e, visitor, currentClassType, jumpLabels, entryPoint)
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
      case UnaryOperator.Minus => compileUnaryMinusExpr(visitor, sop)
      case UnaryOperator.BitwiseNegate => compileUnaryNegateExpr(visitor, sop)
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
  private def compileUnaryMinusExpr(visitor: MethodVisitor, sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = sop match {
    case Float32Op.Neg => visitor.visitInsn(FNEG)
    case Float64Op.Neg => visitor.visitInsn(DNEG)
    case Int8Op.Neg =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2B)
    case Int16Op.Neg =>
      visitor.visitInsn(INEG)
      visitor.visitInsn(I2S)
    case Int32Op.Neg => visitor.visitInsn(INEG)
    case Int64Op.Neg => visitor.visitInsn(LNEG)
    case BigIntOp.Neg =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "negate",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigInteger), false)
    case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
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
  private def compileUnaryNegateExpr(visitor: MethodVisitor, sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = sop match {
    case Int8Op.Not | Int16Op.Not | Int32Op.Not =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(IXOR)
    case Int64Op.Not =>
      visitor.visitInsn(ICONST_M1)
      visitor.visitInsn(I2L)
      visitor.visitInsn(LXOR)
    case BigIntOp.Not =>
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "not",
        AsmOps.getMethodDescriptor(Nil, JvmType.BigInteger), false)
    case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
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
  private def compileArithmeticExpr(e1: Expression,
                                    e2: Expression,
                                    currentClassType: JvmType.Reference,
                                    visitor: MethodVisitor,
                                    jumpLabels: Map[Symbol.LabelSym, Label],
                                    entryPoint: Label,
                                    o: ArithmeticOperator,
                                    sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = {
    if (o == BinaryOperator.Exponentiate) {
      val (castToDouble, castFromDouble) = sop match {
        case Float32Op.Exp => (F2D, D2F)
        case Float64Op.Exp => (NOP, NOP) // already a double
        case Int8Op.Exp | Int16Op.Exp | Int32Op.Exp => (I2D, D2I)
        case Int64Op.Exp => (L2D, D2L)
        case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
      }
      visitor.visitFieldInsn(GETSTATIC, JvmName.ScalaMathPkg.toInternalName, "MODULE$", JvmType.ScalaMathPkg.toDescriptor)
      compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
      visitor.visitInsn(castToDouble)
      compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
      visitor.visitInsn(castToDouble)
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.ScalaMathPkg.toInternalName, "pow",
        AsmOps.getMethodDescriptor(List(JvmType.PrimDouble, JvmType.PrimDouble), JvmType.PrimDouble), false)
      visitor.visitInsn(castFromDouble)
      sop match {
        case Int8Op.Exp => visitor.visitInsn(I2B)
        case Int16Op.Exp => visitor.visitInsn(I2S)
        case Float32Op.Exp | Float64Op.Exp | Int32Op.Exp | Int64Op.Exp => visitor.visitInsn(NOP)
        case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
      }
    } else {
      compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
      compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
      val (intOp, longOp, floatOp, doubleOp, bigIntOp) = o match {
        case BinaryOperator.Plus => (IADD, LADD, FADD, DADD, "add")
        case BinaryOperator.Minus => (ISUB, LSUB, FSUB, DSUB, "subtract")
        case BinaryOperator.Times => (IMUL, LMUL, FMUL, DMUL, "multiply")
        case BinaryOperator.Divide => (IDIV, LDIV, FDIV, DDIV, "divide")
        case BinaryOperator.Modulo => (IREM, LREM, FREM, DREM, "remainder")
        case BinaryOperator.Exponentiate => throw InternalCompilerException("BinaryOperator.Exponentiate already handled.")
      }
      sop match {
        case Float32Op.Add | Float32Op.Sub | Float32Op.Mul | Float32Op.Div | Float32Op.Rem => visitor.visitInsn(floatOp)
        case Float64Op.Add | Float64Op.Sub | Float64Op.Mul | Float64Op.Div | Float64Op.Rem => visitor.visitInsn(doubleOp)
        case Int8Op.Add | Int8Op.Sub | Int8Op.Mul | Int8Op.Div | Int8Op.Rem =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2B)
        case Int16Op.Add | Int16Op.Sub | Int16Op.Mul | Int16Op.Div | Int16Op.Rem =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2S)
        case Int32Op.Add | Int32Op.Sub | Int32Op.Mul | Int32Op.Div | Int32Op.Rem => visitor.visitInsn(intOp)
        case Int64Op.Add | Int64Op.Sub | Int64Op.Mul | Int64Op.Div | Int64Op.Rem => visitor.visitInsn(longOp)
        case BigIntOp.Add | BigIntOp.Sub | BigIntOp.Mul | BigIntOp.Div | BigIntOp.Rem =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, bigIntOp,
            AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.BigInteger), false)
        case StringOp.Concat =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName, "concat",
            AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.String), false)
        case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
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
  private def compileComparisonExpr(e1: Expression,
                                    e2: Expression,
                                    currentClassType: JvmType.Reference,
                                    visitor: MethodVisitor,
                                    jumpLabels: Map[Symbol.LabelSym, Label],
                                    entryPoint: Label,
                                    o: ComparisonOperator,
                                    sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = {
    compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
    compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
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
    sop match {
      case StringOp.Eq | StringOp.Neq =>
        // String can be compared using Object's `equal` method
        visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "equals",
          AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)
        visitor.visitInsn(ICONST_1)
        visitor.visitJumpInsn(intOp, condElse)
      case BoolOp.Eq | BoolOp.Neq =>
        // Bool can be (value) compared for equality.
        visitor.visitJumpInsn(intOp, condElse)
      case Float32Op.Lt | Float32Op.Le | Float32Op.Gt | Float32Op.Ge | Float32Op.Eq | Float32Op.Neq =>
        visitor.visitInsn(floatOp)
        visitor.visitJumpInsn(cmp, condElse)
      case Float64Op.Lt | Float64Op.Le | Float64Op.Gt | Float64Op.Ge | Float64Op.Eq | Float64Op.Neq =>
        visitor.visitInsn(doubleOp)
        visitor.visitJumpInsn(cmp, condElse)
      case CharOp.Lt | CharOp.Le | CharOp.Gt | CharOp.Ge | CharOp.Eq | CharOp.Neq => visitor.visitJumpInsn(intOp, condElse)
      case Int8Op.Lt | Int8Op.Le | Int8Op.Gt | Int8Op.Ge | Int8Op.Eq | Int8Op.Neq => visitor.visitJumpInsn(intOp, condElse)
      case Int16Op.Lt | Int16Op.Le | Int16Op.Gt | Int16Op.Ge | Int16Op.Eq | Int16Op.Neq => visitor.visitJumpInsn(intOp, condElse)
      case Int32Op.Lt | Int32Op.Le | Int32Op.Gt | Int32Op.Ge | Int32Op.Eq | Int32Op.Neq => visitor.visitJumpInsn(intOp, condElse)
      case Int64Op.Lt | Int64Op.Le | Int64Op.Gt | Int64Op.Ge | Int64Op.Eq | Int64Op.Neq =>
        visitor.visitInsn(LCMP)
        visitor.visitJumpInsn(cmp, condElse)
      case BigIntOp.Lt | BigIntOp.Le | BigIntOp.Gt | BigIntOp.Ge | BigIntOp.Eq | BigIntOp.Neq =>
        visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName, "compareTo",
          AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.PrimInt), false)
        visitor.visitInsn(ICONST_0)
        visitor.visitJumpInsn(intOp, condElse)
      case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
    }
    visitor.visitInsn(ICONST_1)
    visitor.visitJumpInsn(GOTO, condEnd)
    visitor.visitLabel(condElse)
    visitor.visitInsn(ICONST_0)
    visitor.visitLabel(condEnd)
  }

  /*
   * Note that LogicalAnd, LogicalOr, and Implication do short-circuit evaluation.
   * Implication and Biconditional are rewritten to their logical equivalents, and then compiled.
   */
  private def compileLogicalExpr(e1: Expression,
                                 e2: Expression,
                                 currentClassType: JvmType.Reference,
                                 visitor: MethodVisitor,
                                 jumpLabels: Map[Symbol.LabelSym, Label],
                                 entryPoint: Label,
                                 o: LogicalOperator)(implicit root: Root, flix: Flix): Unit = o match {
    case BinaryOperator.LogicalAnd =>
      val andFalseBranch = new Label()
      val andEnd = new Label()
      compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
      visitor.visitJumpInsn(IFEQ, andFalseBranch)
      compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
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
      compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
      visitor.visitJumpInsn(IFNE, orTrueBranch)
      compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
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
  private def compileBitwiseExpr(e1: Expression,
                                 e2: Expression,
                                 currentClassType: JvmType.Reference,
                                 visitor: MethodVisitor,
                                 jumpLabels: Map[Symbol.LabelSym, Label],
                                 entryPoint: Label,
                                 o: BitwiseOperator,
                                 sop: SemanticOperator)(implicit root: Root, flix: Flix): Unit = {
    compileExpression(e1, visitor, currentClassType, jumpLabels, entryPoint)
    compileExpression(e2, visitor, currentClassType, jumpLabels, entryPoint)
    val (intOp, longOp, bigintOp) = o match {
      case BinaryOperator.BitwiseAnd => (IAND, LAND, "and")
      case BinaryOperator.BitwiseOr => (IOR, LOR, "or")
      case BinaryOperator.BitwiseXor => (IXOR, LXOR, "xor")
      case BinaryOperator.BitwiseLeftShift => (ISHL, LSHL, "shiftLeft")
      case BinaryOperator.BitwiseRightShift => (ISHR, LSHR, "shiftRight")
    }
    sop match {
      case Int8Op.And | Int8Op.Or | Int8Op.Xor | Int8Op.Shl | Int8Op.Shr =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2B)
      case Int16Op.And | Int16Op.Or | Int16Op.Xor | Int16Op.Shl | Int16Op.Shr =>
        visitor.visitInsn(intOp)
        if (intOp == ISHL) visitor.visitInsn(I2S)
      case Int32Op.And | Int32Op.Or | Int32Op.Xor | Int32Op.Shl | Int32Op.Shr => visitor.visitInsn(intOp)
      case Int64Op.And | Int64Op.Or | Int64Op.Xor | Int64Op.Shl | Int64Op.Shr => visitor.visitInsn(longOp)
      case BigIntOp.And | BigIntOp.Or | BigIntOp.Xor | BigIntOp.Shl | BigIntOp.Shr =>
        visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.BigInteger.toInternalName,
          bigintOp, AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(e2.tpe)), JvmType.BigInteger), false)
      case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
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

}
