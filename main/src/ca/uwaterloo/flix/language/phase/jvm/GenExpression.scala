/*
 * Copyright 2017 Ramin Zarifi
 * Copyright 2021 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.SemanticOperator._
import ca.uwaterloo.flix.language.ast.{MonoType, _}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
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
    case Expression.Unit(loc) =>
      addSourceLine(visitor, loc)
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName,
        BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.toDescriptor)

    case Expression.Null(tpe, loc) =>
      addSourceLine(visitor, loc)
      visitor.visitInsn(ACONST_NULL)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.True(loc) =>
      addSourceLine(visitor, loc)
      visitor.visitInsn(ICONST_1)

    case Expression.False(loc) =>
      addSourceLine(visitor, loc)
      visitor.visitInsn(ICONST_0)

    case Expression.Char(c, loc) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, c)

    case Expression.Float32(f, loc) =>
      addSourceLine(visitor, loc)
      f match {
        case 0f => visitor.visitInsn(FCONST_0)
        case 1f => visitor.visitInsn(FCONST_1)
        case 2f => visitor.visitInsn(FCONST_2)
        case _ => visitor.visitLdcInsn(f)
      }

    case Expression.Float64(d, loc) =>
      addSourceLine(visitor, loc)
      d match {
        case 0d => visitor.visitInsn(DCONST_0)
        case 1d => visitor.visitInsn(DCONST_1)
        case _ => visitor.visitLdcInsn(d)
      }

    case Expression.Int8(b, loc) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, b)

    case Expression.Int16(s, loc) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, s)

    case Expression.Int32(i, loc) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, i)

    case Expression.Int64(l, loc) =>
      addSourceLine(visitor, loc)
      compileInt(visitor, l, isLong = true)

    case Expression.BigInt(ii, loc) =>
      addSourceLine(visitor, loc)
      visitor.visitTypeInsn(NEW, BackendObjType.BigInt.jvmName.toInternalName)
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(ii.toString)
      visitor.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigInt.jvmName.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)

    case Expression.Str(s, loc) =>
      addSourceLine(visitor, loc)
      visitor.visitLdcInsn(s)

    case Expression.Var(sym, tpe, _) =>
      readVar(sym, tpe, visitor)

    case Expression.Closure(sym, closureArgs, fnType, _, _) =>
      // JvmType of the closure
      val jvmType = JvmOps.getClosureClassType(sym, fnType)
      // new closure instance
      visitor.visitTypeInsn(NEW, jvmType.name.toInternalName)
      // Duplicate
      visitor.visitInsn(DUP)
      visitor.visitMethodInsn(INVOKESPECIAL, jvmType.name.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
      // Capturing free args
      for ((arg, i) <- closureArgs.zipWithIndex) {
        val erasedArgType = JvmOps.getErasedJvmType(arg.tpe)
        visitor.visitInsn(DUP)
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, jvmType.name.toInternalName, s"clo$i", erasedArgType.toDescriptor)
      }

    case Expression.ApplyClo(exp, args, tpe, _) =>
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)
      // previous JvmOps functions are already partial pattern matches
      val MonoType.Arrow(_, closureResultType) = exp.tpe
      val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(closureResultType))

      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Casting to JvmType of closure abstract class
      visitor.visitTypeInsn(CHECKCAST, closureAbstractClass.name.toInternalName)
      // retrieving the unique thread object
      visitor.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)
      // Putting args on the Fn class
      for ((arg, i) <- args.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Calling unwind and unboxing
      visitor.visitMethodInsn(INVOKEVIRTUAL, functionInterface.name.toInternalName,
        backendContinuationType.UnwindMethod.name, AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.ApplyDef(name, args, tpe, _) =>
      // JvmType of Def
      val defJvmType = JvmOps.getFunctionDefinitionClassType(name)
      // previous JvmOps function are already partial pattern matches
      val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(tpe))


      // Put the def on the stack
      AsmOps.compileDefSymbol(name, visitor)

      // Putting args on the Fn class
      for ((arg, i) <- args.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, defJvmType.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Calling unwind and unboxing
      visitor.visitMethodInsn(INVOKEVIRTUAL, defJvmType.name.toInternalName, backendContinuationType.UnwindMethod.name,
        AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.ApplyCloTail(exp, args, _, _) =>
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(exp.tpe)
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(exp.tpe)
      // Evaluating the closure
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Casting to JvmType of closure abstract class
      visitor.visitTypeInsn(CHECKCAST, closureAbstractClass.name.toInternalName)
      // retrieving the unique thread object
      visitor.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)
      // Putting args on the Fn class
      for ((arg, i) <- args.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Return the closure
      visitor.visitInsn(ARETURN)

    case Expression.ApplyDefTail(name, args, _, _) =>
      // Type of the function
      val fnType = root.defs(name).tpe
      // Type of the function abstract class
      val functionInterface = JvmOps.getFunctionInterfaceType(fnType)

      // Put the def on the stack
      AsmOps.compileDefSymbol(name, visitor)
      // Putting args on the Fn class
      for ((arg, i) <- args.zipWithIndex) {
        // Duplicate the FunctionInterface
        visitor.visitInsn(DUP)
        // Evaluating the expression
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Return the def
      visitor.visitInsn(ARETURN)

    case Expression.ApplySelfTail(sym, _, actuals, _, _) =>
      // The function abstract class name
      val functionType = JvmOps.getFunctionInterfaceType(root.defs(sym).tpe)
      // Evaluate each argument and put the result on the Fn class.
      for ((arg, i) <- actuals.zipWithIndex) {
        visitor.visitVarInsn(ALOAD, 0)
        // Evaluate the argument and push the result on the stack.
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        visitor.visitFieldInsn(PUTFIELD, functionType.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      // Jump to the entry point of the method.
      visitor.visitJumpInsn(GOTO, entryPoint)

    case Expression.Unary(sop, op, exp, _, _) =>
      sop match {
        case SemanticOperator.ObjectOp.EqNull =>
          compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
          val condElse = new Label()
          val condEnd = new Label()
          visitor.visitJumpInsn(IFNULL, condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_1)
          visitor.visitLabel(condEnd)
        case SemanticOperator.ObjectOp.NeqNull =>
          compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
          val condElse = new Label()
          val condEnd = new Label()
          visitor.visitJumpInsn(IFNULL, condElse)
          visitor.visitInsn(ICONST_1)
          visitor.visitJumpInsn(GOTO, condEnd)
          visitor.visitLabel(condElse)
          visitor.visitInsn(ICONST_0)
          visitor.visitLabel(condEnd)
        case _ =>
          // TODO: Ramin: Must not use `op`, should only use `sop`.
          compileUnaryExpr(exp, currentClass, visitor, lenv0, entryPoint, op, sop)
      }

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

    case Expression.Branch(exp, branches, _, loc) =>
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

    case Expression.JumpTo(sym, _, loc) =>
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
      visitor.visitVarInsn(iStore, sym.getStackOffset + 1)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Jvm Type of the `exp1`
      val jvmType = JvmOps.getJvmType(exp1.tpe)
      // Store instruction for `jvmType`
      val iStore = AsmOps.getStoreInstruction(jvmType)
      // JvmType of the closure
      val cloType = JvmOps.getClosureClassType(defSym, exp1.tpe)

      // Store temp recursive value
      visitor.visitInsn(ACONST_NULL)
      visitor.visitVarInsn(iStore, varSym.getStackOffset + 1)
      // Compile the closure
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      // fix the local and closure reference
      visitor.visitInsn(DUP)
      visitor.visitInsn(DUP)
      visitor.visitFieldInsn(PUTFIELD, cloType.name.toInternalName, s"clo$index", JvmOps.getErasedJvmType(exp1.tpe).toDescriptor)
      // Store the closure locally (maybe not needed?)
      visitor.visitVarInsn(iStore, varSym.getStackOffset + 1)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)

    case Expression.Is(_, tag, exp, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(exp.tpe, tag.name)
      // We get the JvmType of the class for tag
      val classType = JvmOps.getTagClassType(tagInfo)

      // First we compile the `exp`
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // We check if the enum is `instanceof` the class
      visitor.visitTypeInsn(INSTANCEOF, classType.name.toInternalName)

    // Normal Tag
    case Expression.Tag(enum, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Get the tag info.
      val tagInfo = JvmOps.getTagInfo(tpe, tag.name)
      // We get the JvmType of the class for tag
      val classType = JvmOps.getTagClassType(tagInfo)

      ///
      /// Special Case: A tag with a single argument: The unit argument.
      ///
      // TODO: This is a hack until the new and improved backend arrives.
      val whitelistedEnums = List(
        Symbol.mkEnumSym("Comparison"),
        Symbol.mkEnumSym("RedBlackTree.RedBlackTree"),
        Symbol.mkEnumSym("RedBlackTree.Color"),
      )
      if (exp.tpe == MonoType.Unit && whitelistedEnums.contains(enum)) {
        // TODO: This is could introduce errors by if exp has side effects
        // Read the "unitInstance" field of the appropriate class.
        val declaration = classType.name.toInternalName
        val descriptor = classType.toDescriptor
        visitor.visitFieldInsn(GETSTATIC, declaration, "unitInstance", descriptor)
      } else {
        /*
       If the definition of the enum case has a `Unit` field, then it is represented by singleton pattern which means
       there is only one instance of the class initiated as a field. We have to fetch this field instead of instantiating
       a new one.
       */
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

    case Expression.Untag(_, tag, exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)

      // We get the `TagInfo` for the tag
      val tagInfo = JvmOps.getTagInfo(exp.tpe, tag.name)
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

    case Expression.Index(base, offset, tpe, _) =>
      // We get the JvmType of the class for the tuple
      val classType = JvmOps.getTupleClassType(base.tpe.asInstanceOf[MonoType.Tuple])
      // evaluating the `base`
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Retrieving the field `field${offset}`
      visitor.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"field$offset", JvmOps.getErasedJvmType(tpe).toDescriptor)
      // Cast the object to it's type if it's not a primitive
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.Tuple(elms, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the JvmType of the class for the tuple
      val classType = JvmOps.getTupleClassType(tpe.asInstanceOf[MonoType.Tuple])
      // Instantiating a new object of tuple
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicating the class
      visitor.visitInsn(DUP)
      // Evaluating all the elements to be stored in the tuple class
      elms.foreach(compileExpression(_, visitor, currentClass, lenv0, entryPoint))
      // Erased type of `elms`
      val erasedElmTypes = elms.map(_.tpe).map(JvmOps.getErasedJvmType)
      // Descriptor of constructor
      val constructorDescriptor = AsmOps.getMethodDescriptor(erasedElmTypes, JvmType.Void)
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", constructorDescriptor, false)

    case Expression.RecordEmpty(_, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the JvmType of the class for the RecordEmpty
      val classType = JvmOps.getRecordEmptyClassType()
      // Instantiating a new object of tuple
      visitor.visitFieldInsn(GETSTATIC, classType.name.toInternalName, BackendObjType.RecordEmpty.InstanceField.name, classType.toDescriptor)

    case Expression.RecordSelect(exp, field, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)

      // Get the correct record extend class, given the expression type 'tpe'
      // We get the JvmType of the extended record class to retrieve the proper field
      val classType = JvmOps.getRecordType(tpe)

      // We get the JvmType of the record interface
      val interfaceType = JvmOps.getRecordInterfaceType()

      val backendRecordExtendType = BackendObjType.RecordExtend(field.name, BackendType.toErasedBackendType(tpe), BackendObjType.RecordEmpty.toTpe)

      //Compile the expression exp (which should be a record), as we need to have on the stack a record in order to call
      //lookupField
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

      //Push the desired label of the field we want get of the record onto the stack
      visitor.visitLdcInsn(field.name)

      //Invoke the lookupField method on the record. (To get the proper record object)
      visitor.visitMethodInsn(INVOKEINTERFACE, interfaceType.name.toInternalName, "lookupField",
        AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), true)

      //Cast to proper record extend class
      visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)

      //Retrieve the value field  (To get the proper value)
      visitor.visitFieldInsn(GETFIELD, classType.name.toInternalName, backendRecordExtendType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)

      // Cast the field value to the expected type.
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.RecordExtend(field, value, rest, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the JvmType of the class for the record extend
      val classType = JvmOps.getRecordExtendClassType(tpe)

      // We get the JvmType of the record interface
      val interfaceType = JvmOps.getRecordInterfaceType()

      // previous functions are already partial matches
      val MonoType.RecordExtend(_, recordValueType, _) = tpe
      val backendRecordExtendType = BackendObjType.RecordExtend(field.name, BackendType.toErasedBackendType(recordValueType), BackendObjType.RecordEmpty.toTpe)

      // Instantiating a new object of tuple
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      visitor.visitInsn(DUP)
      // Invoking the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)

      //Put the label of field (which is going to be the extension).
      visitor.visitInsn(DUP)
      visitor.visitLdcInsn(field.name)
      visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.LabelField.name, BackendObjType.String.toDescriptor)

      //Put the value of the field onto the stack, since it is an expression we first need to compile it.
      visitor.visitInsn(DUP)
      compileExpression(value, visitor, currentClass, lenv0, entryPoint)
      visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.ValueField.name, JvmOps.getErasedJvmType(value.tpe).toDescriptor)

      //Put the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
      visitor.visitInsn(DUP)
      compileExpression(rest, visitor, currentClass, lenv0, entryPoint)
      visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRecordExtendType.RestField.name, interfaceType.toDescriptor)

    case Expression.RecordRestrict(field, rest, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the JvmType of the record interface
      val interfaceType = JvmOps.getRecordInterfaceType()

      //Push the value of the rest of the record onto the stack, since it's an expression we need to compile it first.
      compileExpression(rest, visitor, currentClass, lenv0, entryPoint)
      //Push the label of field (which is going to be the removed/restricted).
      visitor.visitLdcInsn(field.name)

      // Invoking the restrictField method
      visitor.visitMethodInsn(INVOKEINTERFACE, interfaceType.name.toInternalName, BackendObjType.Record.RestrictFieldMethod.name,
        AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), true)


    case Expression.ArrayLit(elms, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We push the 'length' of the array on top of stack
      compileInt(visitor, elms.length, isLong = false)
      // We get the inner type of the array
      val jvmType = JvmOps.getJvmType(tpe.asInstanceOf[MonoType.Array].tpe)
      // Instantiating a new array of type jvmType
      jvmType match {
        case ref: JvmType.Reference => // Happens if the inner type is an object type
          visitor.visitTypeInsn(ANEWARRAY, ref.name.toInternalName)
        case _ => // Happens if the inner type is a primitive type
          visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
      }
      // For each element we generate code to store it into the array
      for (i <- elms.indices) {
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
      val jvmType = JvmOps.getErasedJvmType(tpe.asInstanceOf[MonoType.Array].tpe)
      // Evaluating the value of the 'default element'
      compileExpression(elm, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'length' of the array
      compileExpression(len, visitor, currentClass, lenv0, entryPoint)
      // Instantiating a new array of type jvmType
      if (jvmType == JvmType.Object) { // Happens if the inner type is an object type
        visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
      } else { // Happens if the inner type is a primitive type
        visitor.visitIntInsn(NEWARRAY, AsmOps.getArrayTypeCode(jvmType))
      }
      if (jvmType == JvmType.PrimLong || jvmType == JvmType.PrimDouble) { // Happens if the inner type is Int64 or Float64
        // Duplicates the 'array reference' three places down the stack
        visitor.visitInsn(DUP_X2)
        // Duplicates the 'array reference' three places down the stack
        visitor.visitInsn(DUP_X2)
        // Pops the 'ArrayRef' at the top of the stack
        visitor.visitInsn(POP)
      } else {
        // Duplicates the 'array reference' two places down the stack
        visitor.visitInsn(DUP_X1)
        // Swaps the 'array reference' and 'default element'
        visitor.visitInsn(SWAP)
      }
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
      visitor.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
      // Evaluating the 'index' to load from
      compileExpression(index, visitor, currentClass, lenv0, entryPoint)
      // Loads the 'element' at the given 'index' from the 'array'
      // with the load instruction corresponding to the loaded element
      visitor.visitInsn(AsmOps.getArrayLoadInstruction(jvmType))

    case Expression.ArrayStore(base, index, elm, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the jvmType of the element to be stored
      val jvmType = JvmOps.getErasedJvmType(elm.tpe)
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Cast the object to Array
      visitor.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
      // Evaluating the 'index' to be stored in
      compileExpression(index, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'element' to be stored
      compileExpression(elm, visitor, currentClass, lenv0, entryPoint)
      // Stores the 'element' at the given 'index' in the 'array'
      // with the store instruction corresponding to the stored element
      visitor.visitInsn(AsmOps.getArrayStoreInstruction(jvmType))
      // Since the return type is 'unit', we put an instance of 'unit' on top of the stack
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

    case Expression.ArrayLength(base, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the inner type of the array
      val jvmType = JvmOps.getErasedJvmType(base.tpe.asInstanceOf[MonoType.Array].tpe)
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Cast the object to array
      visitor.visitTypeInsn(CHECKCAST, AsmOps.getArrayType(jvmType))
      // Pushes the 'length' of the array on top of stack
      visitor.visitInsn(ARRAYLENGTH)

    case Expression.ArraySlice(base, startIndex, endIndex, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // We get the inner type of the array
      val jvmType = JvmOps.getErasedJvmType(base.tpe.asInstanceOf[MonoType.Array].tpe)
      // Evaluating the 'base'
      compileExpression(base, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'startIndex'
      compileExpression(startIndex, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the 'endIndex'
      compileExpression(endIndex, visitor, currentClass, lenv0, entryPoint)
      // Swaps the startIndex and 'endIndex'
      visitor.visitInsn(SWAP)
      // Duplicates the 'startIndex' two places down the stack
      visitor.visitInsn(DUP_X1)
      // Subtracts the 'startIndex' from the 'endIndex' (leaving the 'length' of the array)
      visitor.visitInsn(ISUB)
      // Duplicates the 'length'
      visitor.visitInsn(DUP)
      // Instantiating a new array of type jvmType
      if (jvmType == JvmType.Object) { // Happens if the inner type is an object type
        visitor.visitTypeInsn(ANEWARRAY, BackendObjType.JavaObject.jvmName.toInternalName)
      } else { // Happens if the inner type is a primitive type
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
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.System.toInternalName, "arraycopy", AsmOps.getMethodDescriptor(List(JvmType.Object, JvmType.PrimInt, JvmType.Object, JvmType.PrimInt, JvmType.PrimInt), JvmType.Void), false)
      // Swaps 'new array reference' and 'length'
      visitor.visitInsn(SWAP)
      // Pops the 'length' - leaving 'new array reference' top of stack
      visitor.visitInsn(POP)

    case Expression.Ref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // JvmType of the reference class
      val classType = JvmOps.getRefClassType(tpe)

      // the previous function is already partial
      val MonoType.Ref(refValueType) = tpe
      val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

      // Create a new reference object
      visitor.visitTypeInsn(NEW, classType.name.toInternalName)
      // Duplicate it since one instance will get consumed by constructor
      visitor.visitInsn(DUP)
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
      // Duplicate it since one instance will get consumed by putfield
      visitor.visitInsn(DUP)
      // Evaluate the underlying expression
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // Erased type of the value of the reference
      val valueErasedType = JvmOps.getErasedJvmType(tpe.asInstanceOf[MonoType.Ref].tpe)
      // set the field with the ref value
      visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRefType.ValueField.name, valueErasedType.toDescriptor)

    case Expression.Deref(exp, tpe, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the exp
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      // JvmType of the reference class
      val classType = JvmOps.getRefClassType(exp.tpe)

      // the previous function is already partial
      val MonoType.Ref(refValueType) = exp.tpe
      val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

      // Cast the ref
      visitor.visitTypeInsn(CHECKCAST, classType.name.toInternalName)
      // Dereference the expression
      visitor.visitFieldInsn(GETFIELD, classType.name.toInternalName, backendRefType.ValueField.name, JvmOps.getErasedJvmType(tpe).toDescriptor)
      // Cast underlying value to the correct type if the underlying type is Object
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.Assign(exp1, exp2, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      // Evaluate the reference address
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      // Evaluating the value to be assigned to the reference
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
      // JvmType of the reference class
      val classType = JvmOps.getRefClassType(exp1.tpe)

      // the previous function is already partial
      val MonoType.Ref(refValueType) = exp1.tpe
      val backendRefType = BackendObjType.Ref(BackendType.toErasedBackendType(refValueType))

      // Invoke `setValue` method to set the value to the given number
      visitor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendRefType.ValueField.name, JvmOps.getErasedJvmType(exp2.tpe).toDescriptor)
      // Since the return type is unit, we put an instance of unit on top of the stack
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

    case Expression.Cast(exp, tpe, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.TryCatch(exp, rules, _, loc) =>
      // Add source line number for debugging.
      addSourceLine(visitor, loc)

      // Introduce a label for before the try block.
      val beforeTryBlock = new Label()

      // Introduce a label for after the try block.
      val afterTryBlock = new Label()

      // Introduce a label after the try block and after all catch rules.
      val afterTryAndCatch = new Label()

      // Introduce a label for each catch rule.
      val rulesAndLabels = rules map {
        rule => rule -> new Label()
      }

      // Emit a try catch block for each catch rule.
      for ((CatchRule(_, clazz, _), handlerLabel) <- rulesAndLabels) {
        visitor.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, asm.Type.getInternalName(clazz))
      }

      // Emit code for the try block.
      visitor.visitLabel(beforeTryBlock)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitLabel(afterTryBlock)
      visitor.visitJumpInsn(GOTO, afterTryAndCatch)

      // Emit code for each catch rule.
      for ((CatchRule(sym, _, body), handlerLabel) <- rulesAndLabels) {
        // Emit the label.
        visitor.visitLabel(handlerLabel)

        // Store the exception in a local variable.
        val istore = AsmOps.getStoreInstruction(JvmType.Object)
        visitor.visitVarInsn(istore, sym.getStackOffset + 1)

        // Emit code for the handler body expression.
        compileExpression(body, visitor, currentClass, lenv0, entryPoint)
        visitor.visitJumpInsn(GOTO, afterTryAndCatch)
      }

      // Add the label after both the try and catch rules.
      visitor.visitLabel(afterTryAndCatch)

    case Expression.InvokeConstructor(constructor, args, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)
      val descriptor = asm.Type.getConstructorDescriptor(constructor)
      val declaration = asm.Type.getInternalName(constructor.getDeclaringClass)
      // Create a new object of the declaration type
      visitor.visitTypeInsn(NEW, declaration)
      // Duplicate the reference since the first argument for a constructor call is the reference to the object
      visitor.visitInsn(DUP)
      // Evaluate arguments left-to-right and push them onto the stack.
      for (arg <- args) {
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        // Cast the argument to the right type.
        arg.tpe match {
          // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
          case MonoType.Array(MonoType.Float32) => visitor.visitTypeInsn(CHECKCAST, "[F")
          case MonoType.Array(MonoType.Float64) => visitor.visitTypeInsn(CHECKCAST, "[D")
          case MonoType.Array(MonoType.Int8) => visitor.visitTypeInsn(CHECKCAST, "[B")
          case MonoType.Array(MonoType.Int16) => visitor.visitTypeInsn(CHECKCAST, "[S")
          case MonoType.Array(MonoType.Int32) => visitor.visitTypeInsn(CHECKCAST, "[I")
          case MonoType.Array(MonoType.Int64) => visitor.visitTypeInsn(CHECKCAST, "[J")
          case MonoType.Native(clazz) =>
            val argType = asm.Type.getInternalName(clazz)
            visitor.visitTypeInsn(CHECKCAST, argType)
          case _ => // nop
        }
      }
      // Call the constructor
      visitor.visitMethodInsn(INVOKESPECIAL, declaration, "<init>", descriptor, false)

    case Expression.InvokeMethod(method, exp, args, _, loc) =>
      // Adding source line number for debugging
      addSourceLine(visitor, loc)

      // Evaluate the receiver object.
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      val thisType = asm.Type.getInternalName(method.getDeclaringClass)
      visitor.visitTypeInsn(CHECKCAST, thisType)

      // Retrieve the signature.
      val signature = method.getParameterTypes

      // Evaluate arguments left-to-right and push them onto the stack.
      for ((arg, argType) <- args.zip(signature)) {
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        if (!argType.isPrimitive) {
          // NB: Really just a hack because the backend does not support array JVM types properly.
          visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
        } else {
          arg.tpe match {
            // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
            case MonoType.Array(MonoType.Float32) => visitor.visitTypeInsn(CHECKCAST, "[F")
            case MonoType.Array(MonoType.Float64) => visitor.visitTypeInsn(CHECKCAST, "[D")
            case MonoType.Array(MonoType.Int8) => visitor.visitTypeInsn(CHECKCAST, "[B")
            case MonoType.Array(MonoType.Int16) => visitor.visitTypeInsn(CHECKCAST, "[S")
            case MonoType.Array(MonoType.Int32) => visitor.visitTypeInsn(CHECKCAST, "[I")
            case MonoType.Array(MonoType.Int64) => visitor.visitTypeInsn(CHECKCAST, "[J")
            case _ => // nop
          }
        }
      }
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)

      // Check if we are invoking an interface or class.
      if (method.getDeclaringClass.isInterface) {
        visitor.visitMethodInsn(INVOKEINTERFACE, declaration, name, descriptor, true)
      } else {
        visitor.visitMethodInsn(INVOKEVIRTUAL, declaration, name, descriptor, false)
      }

      // If the method is void, put a unit on top of the stack
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
      }

    case Expression.InvokeStaticMethod(method, args, _, loc) =>
      addSourceLine(visitor, loc)
      val signature = method.getParameterTypes
      for ((arg, argType) <- args.zip(signature)) {
        compileExpression(arg, visitor, currentClass, lenv0, entryPoint)
        if (!argType.isPrimitive) {
          // NB: Really just a hack because the backend does not support array JVM types properly.
          visitor.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(argType))
        } else {
          arg.tpe match {
            // NB: This is not exhaustive. In the new backend we should handle all types, including multidim arrays.
            case MonoType.Array(MonoType.Float32) => visitor.visitTypeInsn(CHECKCAST, "[F")
            case MonoType.Array(MonoType.Float64) => visitor.visitTypeInsn(CHECKCAST, "[D")
            case MonoType.Array(MonoType.Int8) => visitor.visitTypeInsn(CHECKCAST, "[B")
            case MonoType.Array(MonoType.Int16) => visitor.visitTypeInsn(CHECKCAST, "[S")
            case MonoType.Array(MonoType.Int32) => visitor.visitTypeInsn(CHECKCAST, "[I")
            case MonoType.Array(MonoType.Int64) => visitor.visitTypeInsn(CHECKCAST, "[J")
            case _ => // nop
          }
        }
      }
      val declaration = asm.Type.getInternalName(method.getDeclaringClass)
      val name = method.getName
      val descriptor = asm.Type.getMethodDescriptor(method)
      // Check if we are invoking an interface or class.
      if (method.getDeclaringClass.isInterface) {
        visitor.visitMethodInsn(INVOKESTATIC, declaration, name, descriptor, true)
      } else {
        visitor.visitMethodInsn(INVOKESTATIC, declaration, name, descriptor, false)
      }
      if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) {
        visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
      }

    case Expression.GetField(field, exp, tpe, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      val declaration = asm.Type.getInternalName(field.getDeclaringClass)
      visitor.visitFieldInsn(GETFIELD, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

    case Expression.PutField(field, exp1, exp2, _, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
      val declaration = asm.Type.getInternalName(field.getDeclaringClass)
      visitor.visitFieldInsn(PUTFIELD, declaration, field.getName, JvmOps.getJvmType(exp2.tpe).toDescriptor)

      // Push Unit on the stack.
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

    case Expression.GetStaticField(field, tpe, loc) =>
      addSourceLine(visitor, loc)
      val declaration = asm.Type.getInternalName(field.getDeclaringClass)
      visitor.visitFieldInsn(GETSTATIC, declaration, field.getName, JvmOps.getJvmType(tpe).toDescriptor)

    case Expression.PutStaticField(field, exp, _, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      val declaration = asm.Type.getInternalName(field.getDeclaringClass)
      visitor.visitFieldInsn(PUTSTATIC, declaration, field.getName, JvmOps.getJvmType(exp.tpe).toDescriptor)

      // Push Unit on the stack.
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

    case Expression.NewObject(_, tpe, loc) =>
      addSourceLine(visitor, loc)
      val className = JvmName(ca.uwaterloo.flix.language.phase.jvm.JvmName.RootPackage, "HardcodedAnon").toInternalName
      visitor.visitTypeInsn(NEW, className)
      visitor.visitInsn(DUP)
      visitor.visitMethodInsn(INVOKESPECIAL, className, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    case Expression.NewChannel(exp, _, loc) =>
      addSourceLine(visitor, loc)
      visitor.visitTypeInsn(NEW, JvmName.Channel.toInternalName)
      visitor.visitInsn(DUP)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESPECIAL, JvmName.Channel.toInternalName, "<init>", AsmOps.getMethodDescriptor(List(JvmType.PrimInt), JvmType.Void), false)

    case Expression.GetChannel(exp, tpe, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, JvmName.Channel.toInternalName)
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.Channel.toInternalName, "get", AsmOps.getMethodDescriptor(Nil, JvmType.Object), false)
      AsmOps.castIfNotPrimAndUnbox(visitor, JvmOps.getJvmType(tpe))

    case Expression.PutChannel(exp1, exp2, _, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp1, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, JvmName.Channel.toInternalName)
      compileExpression(exp2, visitor, currentClass, lenv0, entryPoint)
      AsmOps.boxIfPrim(visitor, JvmOps.getJvmType(exp2.tpe))
      visitor.visitMethodInsn(INVOKEVIRTUAL, JvmName.Channel.toInternalName, "put", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), false)
      // push unit on the stack
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)


    case Expression.SelectChannel(rules, default, _, loc) =>
      addSourceLine(visitor, loc)
      // Make a new Channel[] containing all channel expressions

      // Calculate the size of the array and initiate it
      compileInt(visitor, rules.size)
      visitor.visitTypeInsn(ANEWARRAY, JvmName.Channel.toInternalName)
      for ((rule, index) <- rules.zipWithIndex) {
        // Dup so we end up with an array on top of the stack
        visitor.visitInsn(DUP)
        // Compile the index
        compileInt(visitor, index)
        // Compile the chan expression 100
        compileExpression(rule.chan, visitor, currentClass, lenv0, entryPoint)
        // Cast the type from Object to Channel
        visitor.visitTypeInsn(CHECKCAST, JvmName.Channel.toInternalName)
        // Store the expression in the array
        visitor.visitInsn(AASTORE)
      }

      // If this select has a default, we want to call select with true
      if (default.isDefined) {
        visitor.visitInsn(ICONST_1)
      } else {
        visitor.visitInsn(ICONST_0)
      }


      // Invoke select in Channel. This puts a SelectChoice on the stack
      visitor.visitMethodInsn(INVOKESTATIC, JvmName.Channel.toInternalName, "select", "([Lca/uwaterloo/flix/runtime/interpreter/Channel;Z)Lca/uwaterloo/flix/runtime/interpreter/SelectChoice;", false)

      // Check if the default case was selected
      val defaultLabel = new Label()
      visitor.visitInsn(DUP)
      visitor.visitFieldInsn(GETFIELD, JvmName.SelectChoice.toInternalName, "defaultChoice", JvmType.PrimBool.toDescriptor)
      // Jump if needed
      visitor.visitJumpInsn(IFNE, defaultLabel)

      // Dup since we need to get the element and the relevant index
      visitor.visitInsn(DUP)
      // Get the relevant branchNumber and put it on the stack
      visitor.visitFieldInsn(GETFIELD, JvmName.SelectChoice.toInternalName, "branchNumber", JvmType.PrimInt.toDescriptor)

      val labels: List[Label] = rules.map(_ => new Label())
      val lookupErrorLabel: Label = new Label()
      val completedLabel: Label = new Label()
      // Find the correct branch with a table switch
      visitor.visitTableSwitchInsn(0, rules.length - 1, lookupErrorLabel, labels: _*)

      for ((rule, label) <- rules.zip(labels)) {
        visitor.visitLabel(label)
        // The SelectChoice is on top of the stack. We get the element of the channel
        visitor.visitFieldInsn(GETFIELD, JvmName.SelectChoice.toInternalName, "element", JvmType.Object.toDescriptor)
        // Jvm Type of the elementType
        val channelType = rule.chan.tpe.asInstanceOf[MonoType.Channel].tpe
        val jvmType = JvmOps.getErasedJvmType(channelType)
        // Unbox if needed for primitives
        AsmOps.castIfNotPrimAndUnbox(visitor, jvmType)
        // Store instruction for `jvmType`
        val iStore = AsmOps.getStoreInstruction(jvmType)
        // Extend the environment with the element from the channel
        visitor.visitVarInsn(iStore, rule.sym.getStackOffset + 1)
        // Finally compile the body of the selected rule
        compileExpression(rule.exp, visitor, currentClass, lenv0, entryPoint)
        // Jump out of the cases so we do not fall through to the next one
        visitor.visitJumpInsn(GOTO, completedLabel)
      }

      // TableSwitch instruction jumps here if "0 <= index < rules.length" is not satisfied.
      visitor.visitLabel(lookupErrorLabel)
      // TODO: throw a better error
      // Throw exception
      visitor.visitInsn(ACONST_NULL)
      visitor.visitInsn(ATHROW)

      // Place the default label
      visitor.visitLabel(defaultLabel)
      // Pop the SelectChoice
      visitor.visitInsn(POP)
      // If we have a default case we can compile that, otherwise we write
      // an error we will never hit to satisfy the jvm
      if (default.isDefined) {
        compileExpression(default.get, visitor, currentClass, lenv0, entryPoint)
      } else {
        visitor.visitInsn(ACONST_NULL)
        visitor.visitInsn(ATHROW)
      }

      // Jump here if the correct rule has been evaluated
      visitor.visitLabel(completedLabel)

    case Expression.Spawn(exp, _, loc) =>
      addSourceLine(visitor, loc)
      // Compile the expression, putting a function implementing the Runnable interface on the stack
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, JvmName.Runnable.toInternalName)
      // make a thread and run it
      visitor.visitTypeInsn(NEW, "java/lang/Thread")
      visitor.visitInsn(DUP_X1)
      visitor.visitInsn(SWAP)
      visitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Thread", "<init>", s"(${JvmName.Runnable.toDescriptor})${JvmType.Void.toDescriptor}", false)
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Thread", "start", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
      // Put a Unit value on the stack
      visitor.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

    case Expression.Lazy(exp, tpe, loc) =>
      // Add source line numbers for debugging.
      addSourceLine(visitor, loc)

      // Find the Lazy class name (Lazy$tpe).
      val classType = JvmOps.getLazyClassType(tpe.asInstanceOf[MonoType.Lazy]).name.toInternalName

      // Make a new lazy object and dup it to leave it on the stack.
      visitor.visitTypeInsn(NEW, classType)
      visitor.visitInsn(DUP)

      // Compile the thunked expression and call new Lazy$erased_tpe(expression).
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESPECIAL, classType, "<init>", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), false)

    case Expression.Force(exp, _, loc) =>
      // Add source line numbers for debugging.
      addSourceLine(visitor, loc)

      // Find the Lazy class type (Lazy$tpe) and the inner value type.
      val classMonoType = exp.tpe.asInstanceOf[MonoType.Lazy]
      val classType = JvmOps.getLazyClassType(classMonoType)
      val internalClassType = classType.name.toInternalName
      val MonoType.Lazy(tpe) = classMonoType
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Emit code for the lazy expression.
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)

      // Lazy$tpe is expected.
      visitor.visitTypeInsn(CHECKCAST, internalClassType)

      // Dup for later lazy.value or lazy.force(context)
      visitor.visitInsn(DUP)
      // Get expression
      visitor.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)
      val alreadyInit = new Label()
      val end = new Label()
      // If expression == null the we just use lazy.value, otherwise lazy.force(context)
      visitor.visitJumpInsn(IFNULL, alreadyInit)

      // Call force().
      visitor.visitMethodInsn(INVOKEVIRTUAL, internalClassType, "force", AsmOps.getMethodDescriptor(Nil, erasedType), false)
      // goto the cast to undo erasure
      visitor.visitJumpInsn(GOTO, end)

      visitor.visitLabel(alreadyInit)
      // Retrieve the erased value
      visitor.visitFieldInsn(GETFIELD, internalClassType, "value", erasedType.toDescriptor)

      visitor.visitLabel(end)
      // The result of force is a generic object so a cast is needed.
      AsmOps.castIfNotPrim(visitor, JvmOps.getJvmType(tpe))

    case Expression.HoleError(sym, _, loc) =>
      addSourceLine(visitor, loc)
      AsmOps.compileThrowHoleError(visitor, sym.toString, loc)

    case Expression.MatchError(_, loc) =>
      addSourceLine(visitor, loc)
      AsmOps.compileThrowFlixError(visitor, BackendObjType.MatchError.jvmName, loc)

    case Expression.BoxBool(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)


    case Expression.BoxInt8(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false)


    case Expression.BoxInt16(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false)


    case Expression.BoxInt32(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)


    case Expression.BoxInt64(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)


    case Expression.BoxChar(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)


    case Expression.BoxFloat32(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false)


    case Expression.BoxFloat64(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false)

    case Expression.UnboxBool(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)


    case Expression.UnboxInt8(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Character")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)


    case Expression.UnboxInt16(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Short")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false)


    case Expression.UnboxInt32(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Integer")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)

    case Expression.UnboxInt64(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Long")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false)


    case Expression.UnboxChar(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Character")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false)


    case Expression.UnboxFloat32(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Float")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false)


    case Expression.UnboxFloat64(exp, loc) =>
      addSourceLine(visitor, loc)
      compileExpression(exp, visitor, currentClass, lenv0, entryPoint)
      visitor.visitTypeInsn(CHECKCAST, "java/lang/Double")
      visitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false)

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
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, "negate",
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
      visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, "not",
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
        case BinaryOperator.Modulo => (IREM, LREM, FREM, DREM, "remainder")
        case BinaryOperator.Divide => (IDIV, LDIV, FDIV, DDIV, "divide")
        case BinaryOperator.Exponentiate => throw InternalCompilerException("BinaryOperator.Exponentiate already handled.")
      }
      sop match {
        case Float32Op.Add | Float32Op.Sub | Float32Op.Mul | Float32Op.Div => visitor.visitInsn(floatOp)
        case Float64Op.Add | Float64Op.Sub | Float64Op.Mul | Float64Op.Div => visitor.visitInsn(doubleOp)
        case Int8Op.Add | Int8Op.Sub | Int8Op.Mul | Int8Op.Div | Int8Op.Rem =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2B)
        case Int16Op.Add | Int16Op.Sub | Int16Op.Mul | Int16Op.Div | Int16Op.Rem =>
          visitor.visitInsn(intOp)
          visitor.visitInsn(I2S)
        case Int32Op.Add | Int32Op.Sub | Int32Op.Mul | Int32Op.Div | Int32Op.Rem => visitor.visitInsn(intOp)
        case Int64Op.Add | Int64Op.Sub | Int64Op.Mul | Int64Op.Div | Int64Op.Rem => visitor.visitInsn(longOp)
        case BigIntOp.Add | BigIntOp.Sub | BigIntOp.Mul | BigIntOp.Div | BigIntOp.Rem =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, bigIntOp,
            AsmOps.getMethodDescriptor(List(JvmType.BigInteger), JvmType.BigInteger), false)
        case StringOp.Concat =>
          visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.String.jvmName.toInternalName, "concat",
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
      case BinaryOperator.Spaceship => throw InternalCompilerException("Unexpected operator.")
    }
    sop match {
      case StringOp.Eq | StringOp.Neq =>
        // String can be compared using Object's `equal` method
        visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.JavaObject.jvmName.toInternalName, "equals",
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
        visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName, "compareTo",
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
        visitor.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.BigInt.jvmName.toInternalName,
          bigintOp, AsmOps.getMethodDescriptor(List(JvmOps.getJvmType(e2.tpe)), JvmType.BigInteger), false)
      case _ => throw InternalCompilerException(s"Unexpected semantic operator: $sop.")
    }
  }

  /**
    * Generates code to read the given variable symbol and put it on top of the stack.
    */
  private def readVar(sym: Symbol.VarSym, tpe: MonoType, mv: MethodVisitor)(implicit root: Root, flix: Flix): Unit = {
    val jvmType = JvmOps.getErasedJvmType(tpe)
    val iLOAD = AsmOps.getLoadInstruction(jvmType)
    mv.visitVarInsn(iLOAD, sym.getStackOffset + 1)
    AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))
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
