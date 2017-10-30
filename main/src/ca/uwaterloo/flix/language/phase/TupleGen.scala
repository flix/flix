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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.language.phase.CodegenHelper._
import org.objectweb.asm
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._

object TupleGen extends Phase[ExecutableAst.Root, ExecutableAst.Root] {

  /**
    * Generate bytecode for tuples.
    * The steps that we take to generate enums is as follows:
    *
    * 1. Extract all tuple types from definitions.
    * At this step, we use `findTuple` method to extract all tuples from all the definitions available at this stage
    *
    * 2. Group tuples based on representation of their fields.
    * At this step we group tuples that have the same field representation so we only generate one class for them.
    * If a field is a primitive, then it can be represented by it's primitive but if the field is not a primitive then it
    * has to be represented using an object.
    * For example, `(List[Int32], Bool)` and `(Result[Int32,Int32], Bool)` have the same representation since the first field
    * of both of them is an object and the second field is a `Bool`. So we only create one tuple class for both of these
    * tuples.
    *
    * 3. Gather unique tuple representations.
    * At this step, we generate representation of  all the tuple classes that we have to create. If a field is a primitive
    * then we wrap the field inside `WrappedPrimitive` and if the field is not a primitive then we wrap all the types that
    * will be represented using `object` on this tuple inside `WrappedNonPrimitives`.
    * For example for tuples with element type `(Int, Int)`, we represent this with
    * `List(WrappedPrimitive(Int32), WrappedPrimitive(In32))`. If we have to represent tuples of type `(List[Int32], Bool)` and
    * `(Result[Int32,Int32], Bool)` then we represent the class that can represent both of these tuples by
    * `List(WrappedNonPrimitives(List(Result[Int32,Int32], List[Int32]), WrappedPrimitive(Bool)))`
    *
    * 3. Emit code for tuple classes
    * At this step, we emit code for tuple classes.
    * Each tuple class includes a field corresponding to each of the parameters of the tuple which either has the primitive
    * type representing the tuple or they have the general type object. Tuple classes include `getBoxedValue()` method which
    * will return an array of object which each object in the array represent the boxed value of a field of the class. It also
    * include equals, hashCode, toString and a constructor which each parameter on the constructor corresponds to a field of
    * the class.
    */
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    if (flix.options.evaluation == Evaluation.Interpreted) {
      return root.toSuccess
    }

    // 1. Extract all tuple types from definitions.
    val allTuples: List[Type] = root.defs.values.flatMap(x => findTuplesInExps(x.exp) ++ findTuplesInTypes(x.tpe)).toList


    // 2. Group tuples based on representation of their fields.
    val groupedFields: List[List[List[Type]]] = allTuples.map {
      case t => t.typeArguments
    }.groupBy(_.map(typeSpecifier)).values.toList

    // 3. Gather unique tuple representations.
    val wrappedFields: Set[List[WrappedType]] = groupedFields.map { grp =>
      val len = grp.head.length
      (0 until len).map { ind =>
        val underlyings = grp.map(tuple => tuple(ind))
        if (isPrimitive(underlyings.head)) {
          WrappedPrimitive(underlyings.head)
        } else {
          WrappedNonPrimitives(underlyings.toSet)
        }
      }.toList
    }.toSet

    // 4. Emit code for tuple classes
    val tupleClassByteCode: Map[TupleClassName, Array[Byte]] = wrappedFields.map { fields =>
      TupleClassName(fields) -> compileTuple(fields)
    }.toMap

    val e = System.nanoTime() - t
    root.copy(byteCodes = root.byteCodes.copy(tupleByteCode = tupleClassByteCode), time = root.time.copy(tupleGen = e)).toSuccess
  }

  /**
    * This method creates the class for each tuple.
    * The class generated by this method will implement `Tuple` which includes a single method: `getBoxedValue()`
    * Here, we first instantiate the visitor required to emit the code.
    *
    * Then we create the name of the class to be generated and store the result in `className`
    *
    * We then define the super of this class (Object is the supper here) and interfaces which this class implements (Tuple).
    * Then using super and interfaces we will create the class header.
    *
    * We then precede to creating a field for each element of the tuple on the class. We use `compileField` helper with
    * name = `field${ind}` with `ind` being the index of the element in tuple and with descriptor obtained from `getWrappedTypeDescriptor`.
    * For example, if the second element of type is of type `WrappedPrimitive(Bool)`, we create the following
    * field on the class:
    *
    * public boolean field1;
    *
    * and if the 5th element of the tuple if of type `WrappedNonPrimitives(Set(..))` we create the following field on the class:
    *
    * public Object field4;
    *
    * Then we precede with generating the code for constructor. Number of arguments on this constructor is equal number
    * of elements in the tuple. Each of these arguments will be used to set a field on the class.
    * For example for tuple (Char, Int8) we create the following constructor:
    *
    * public Tuple(char var1, byte var2) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    * }
    *
    * Then we generate the `getBoxedValue()` method which will return an array containing all the elements of the represented
    * tuple but all elements are boxed if their type is not a primitive.
    *
    * Next we generate the `equals(Obj)` method which will return true if the object that implement the method `equals(Obj)`
    * is equal to `Obj` and will return `false` otherwise. For doing this, at first we check that `Obj` is instance of
    * the class that we are generating. Then we will check that the value of each field is equal for both `Obj` and `this`.
    * If the field has a primitive type then we will use `==` to compare the field otherwise we will invoke `equals(Obj)`
    * on one field with the other field as the parameter of `equals` method.
    * For example for `(WrappedPrimitive(Bool), WrappedNonPrimitives(Set(..)))` we will create the following `equals` method:
    *
    * public boolean equals(Object var1) {
    * return var1 instanceof Tuple && ((Tuple)var1).field0 == this.field0 && ((Tuple)var1).field1.equals(this.field1);
    * }
    *
    * Then we generate the `hashCode()` of the object. The hash value of the tuple is defined as follows:
    * First we initialize the hashValue to be 0. Then we loop over elements of the tuple and at each step of the loop,
    * we first multiply the current value of hash by 7, then we add the hashCode of the current element at this step of
    * the loop to the hashValue if the element is an object and if the element is not a primitive, then we will cast
    * the value of the element to int and add it to hashValue.
    * For example, for `(WrappedPrimitive(Int32), WrappedNonPrimitives(Set(..)), WrappedPrimitive(Int32))` we will
    * generate the following `hashCode()` method:
    *
    * public int hashCode() {
    * return ((0 * 7 + this.field0) * 7 + this.field1.hashCode()) * 7 + this.field2;
    * }
    *
    * Finally, we will generate the `toString()` method of the class. For each tuple (x_1, x_2, ..., x_n) it will return
    * the string `Tuple(rep(x_1), rep(x_2), ..., rep(x_n))` which `rep(x_i)` is string representation of element `x_i` of
    * the tuple which if the element is primitive, we use `valueOf` static method on string class to get string representation
    * of the element and if the element is an object then we call `toString` method on the object.
    * For example, for `((WrappedPrimitive(Int32), WrappedNonPrimitives(Set(..)), WrappedPrimitive(Int32))` we will generate
    * the following `toString()` method:
    *
    * public String toString() {
    * return "Tuple(".concat(String.valueOf(this.field0)).concat(", ").concat(this.field1.toString()).concat(", ").concat(String.valueOf(this.field2)).concat(")");
    * }
    *
    * @param fields fields of the tuple to be generated
    * @return bytecode of the class representing the tuple
    */
  private def compileTuple(fields: List[WrappedType]): Array[Byte] = {
    /*
     * Initialize the class writer. We override `getCommonSuperClass` method because `asm` implementation of this
     * function requires types to loaded so that they can be compared to each other.
     */
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
      override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
        asm.Type.getInternalName(Constants.objectClass)
      }
    }

    // Qualified name of the class that will be generated by this method
    val qualName = TupleClassName(fields)

    // Super descriptor
    val superDescriptor = asm.Type.getInternalName(Constants.objectClass)

    // Descriptors of implemented interfaces
    val interfaceDesctiptors = Array(asm.Type.getInternalName(Constants.tupleClass))

    // Initialize the visitor to create a class.
    visitor.visit(JavaVersion, ACC_PUBLIC + ACC_FINAL, decorate(qualName), null, superDescriptor, interfaceDesctiptors)

    // Source of the class
    visitor.visitSource(decorate(qualName), null)

    fields.zipWithIndex.foreach { case (field, ind) =>
      // Descriptor of the field
      val desc = getWrappedTypeDescriptor(field)

      // Name of the field
      val fieldName = s"field$ind"

      // Defining fields of the tuple
      compileField(visitor, fieldName, desc, isStatic = false, isPrivate = true)

      // Emitting getter for each field
      compileGetFieldMethod(visitor, qualName, desc, fieldName, s"getIndex$ind", getReturnInsn(field))

      // Emitting setter for each field
      compileSetFieldMethod(visitor, qualName, desc, fieldName, s"setIndex$ind", getLoadInstruction(field))
    }

    // Emit the code for the constructor
    compileTupleConstructor(visitor, qualName, fields)

    // Emit the code for `getBoxedValue()` method
    compileGetBoxedValueMethod(visitor, qualName, fields)

    // Emit the code for `equals(obj)` method
    compileEqualsMethod(visitor, qualName, fields)

    // Emit the code for `hashCode()` method
    compileHashCodeMethod(visitor, qualName, fields)

    // Emit the code for `toString()` method
    compileToStringMethod(visitor, qualName, fields)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * This method generates the constructor for the tuple class. Number of arguments on this constructor is equal number
    * of elements in the tuple and each argument corresponds to an element of the tuple with the appropriate type.
    * For example for tuple (Char, Int8) we create the following constructor:
    *
    * public Tuple(char var1, byte var2) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    * }
    *
    * @param visitor   ClassWrite for emitting the code
    * @param className Qualified name of the class of tuple
    * @param fields    fields on the tuple class
    */
  private def compileTupleConstructor(visitor: ClassWriter, className: TupleClassName, fields: List[WrappedType]) = {
    val desc = fields.map(getWrappedTypeDescriptor).mkString

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", s"($desc)V", null, null)
    val clazz = Constants.objectClass
    val ctor = clazz.getConstructor()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, asm.Type.getInternalName(clazz), "<init>",
      asm.Type.getConstructorDescriptor(ctor), false)

    var offset: Int = 1
    fields.zipWithIndex.foreach { case (field, ind) =>
      val desc = getWrappedTypeDescriptor(field)
      val iLoad = getLoadInstruction(field)

      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitVarInsn(iLoad, offset)
      constructor.visitFieldInsn(PUTFIELD, decorate(className), s"field$ind", desc)

      field match {
        case WrappedPrimitive(Type.Int64) | WrappedPrimitive(Type.Float64) => offset += 2
        case _ => offset += 1
      }
    }
    // Return
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  /**
    * This method generates the `equals(Obj)` method which will return true if the object that implement the method `equals(Obj)`
    * is equal to `Obj` and will return `false` otherwise. For doing this, at first we check that `Obj` is instance of
    * the class that we are generating. Then we will check that the value of each field is equal for both `Obj` and `this`.
    * If the field has a primitive type then we will use `==` to compare the field otherwise we will invoke `equals(Obj)`
    * on one field with the other field as the parameter of `equals` method.
    * For example for `(WrappedPrimitive(Bool), WrappedNonPrimitives(Set(..)))` we will create the following `equals` method:
    *
    * public boolean equals(Object var1) {
    * return var1 instanceof Tuple && ((Tuple)var1).field0 == this.field0 && ((Tuple)var1).field1.equals(this.field1);
    * }
    *
    * @param visitor   ClassWriter for emitting the code to the class
    * @param className Qualified name of the class
    * @param fields    fields of the class
    */
  private def compileEqualsMethod(visitor: ClassWriter, className: TupleClassName, fields: List[WrappedType]) = {
    val clazz = Constants.objectClass

    // label for when the object is not instanceof tag case
    val instNotEq = new Label()
    // label for when the result of comparing an element of a tuple is false
    val valueNotEq = new Label()
    // MethodWriter to emit the code for method body
    val method = visitor.visitMethod(ACC_PUBLIC, "equals", s"(${asm.Type.getDescriptor(clazz)})Z", null, null)

    method.visitCode()

    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(INSTANCEOF, decorate(className)) // compare the types
    method.visitJumpInsn(IFEQ, instNotEq) // if types don't match go to `instNotEq`

    // We cast the argument to current type only once, then we just duplicate the top of the stack.
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(CHECKCAST, decorate(className)) // cast to the current class

    // Now we chack that all elements of tuple have equal values
    fields.zipWithIndex.foreach { case (field, ind) =>
      val desc = getWrappedTypeDescriptor(field)

      method.visitInsn(DUP)
      method.visitFieldInsn(GETFIELD, decorate(className), s"field$ind", desc)
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(className), s"field$ind", desc)
      branchIfNotEqual(method, field, valueNotEq)
    }
    method.visitInsn(POP) // Popping the casted version of the argument
    method.visitInsn(ICONST_1)
    method.visitInsn(IRETURN)
    method.visitLabel(valueNotEq) // if the code reaches here, it means that underlying values were not equal
    method.visitInsn(POP) // Popping the casted version of the argument
    method.visitLabel(instNotEq) // if we jump to this label it means the `instanceof` has returned false.
    method.visitInsn(ICONST_0)

    // Return
    method.visitInsn(IRETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(65535, 65535)
    method.visitEnd()
  }

  /**
    * The hash value of the tuple is defined as follows:
    * For tuple `(x_1, x_2, ..., x_n)` we define the hash of this tuple to be
    * `hash(x_1) * 7^{n - 1} + hash(x_2) * 7^{n - 2} + ... + hash(x_n)`
    * we define `hash(x_i)` to be `x_i` casted to `int` if `x_i` is a primitive or `hashCode` of the value if the value
    * is an object. Final result which is an integer will be return on invocation of this method.
    * For example, for `(WrappedPrimitive(Int32), WrappedNonPrimitive(Set(..)), WrappedPrimitive(Int32))` we will generate
    * the following `hashCode()` method:
    *
    * public int hashCode() {
    * return ((0 * 7 + this.field0) * 7 + this.field1.hashCode()) * 7 + this.field2;
    * }
    *
    * @param visitor   ClassWriter to emit method to the class
    * @param className Qualified name of the class
    * @param fields    Fields of the class
    */
  private def compileHashCodeMethod(visitor: ClassWriter, className: TupleClassName, fields: List[WrappedType]) = {
    // header of the `hashCode` function
    val method = visitor.visitMethod(ACC_PUBLIC, "hashCode", "()I", null, null)

    method.visitCode()
    // Initial value of the accumulator
    method.visitInsn(ICONST_0)

    // Now we loop over fields to compute the hash value
    fields.zipWithIndex.foreach { case (field, ind) =>
      // Multiplying the current hash value by 7
      method.visitLdcInsn(7)
      method.visitInsn(IMUL)

      // descriptor of the current field
      val desc = getWrappedTypeDescriptor(field)

      // Fetching the field
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(className), s"field$ind", desc)

      // Getting the hashCode of the field
      getHashCodeOrConvertToInt(method, field)

      // Adding the hash code to the accumulator
      method.visitInsn(IADD)
    }

    // Returning the hash
    method.visitInsn(IRETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * This method will generate `toString()` method of the class. For each tuple (x_1, x_2, ..., x_n) it will return
    * the string `Tuple(rep(x_1), rep(x_2), ..., rep(x_n))` which `rep(x_i)` is string representation of element `x_i` of
    * the tuple which if the element is primitive, we use `valueOf` static method on string class to get string representation
    * of the element and if the element is an object then we call `toString` method on the object.
    * For example, for `(WrappedPrimitive(Int32), WrappedNonPrimitive(Set(..)), WrappedPrimitive(Int32))` we will
    * generate the following `toString()` method:
    *
    * public String toString() {
    * return "Tuple(".concat(String.valueOf(this.field0)).concat(", ").concat(this.field1.toString()).concat(", ").concat(String.valueOf(this.field2)).concat(")");
    * }
    *
    * @param visitor   ClassWriter to emit method to the class
    * @param className Qualified name of the class
    * @param fields    Fields of the class
    */
  private def compileToStringMethod(visitor: ClassWriter, className: TupleClassName, fields: List[WrappedType]) = {
    val stringInternalName = asm.Type.getInternalName(Constants.stringClass)
    val stringConcatMethod = Constants.stringClass.getMethod("concat", Constants.stringClass)

    // Headers of the method
    val method = visitor.visitMethod(ACC_PUBLIC, "toString", s"()${asm.Type.getDescriptor(Constants.stringClass)}", null, null)

    method.visitCode()

    // Initial accumulator of the result
    method.visitLdcInsn("(")

    // We loop over each field, convert the field to string and concat the result to the accumulator
    fields.zipWithIndex.foreach { case (field, ind) =>
      // descriptor of the field
      val desc = getWrappedTypeDescriptor(field)

      // Fetching the field
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(className), s"field$ind", desc)

      // Converting the field to string
      javaValueToString(method, field)

      // Concatenating the string to the rest of the accumulator
      method.visitMethodInsn(INVOKEVIRTUAL, stringInternalName, stringConcatMethod.getName,
        asm.Type.getMethodDescriptor(stringConcatMethod), false)

      // If this is not the last element of the tuple, then concat the separator `, ` to the accumulated string on the stack
      if (ind < fields.length - 1) {
        method.visitLdcInsn(", ")
        method.visitMethodInsn(INVOKEVIRTUAL, stringInternalName, stringConcatMethod.getName,
          asm.Type.getMethodDescriptor(stringConcatMethod), false)
      }
    }

    method.visitLdcInsn(")")
    method.visitMethodInsn(INVOKEVIRTUAL, stringInternalName, stringConcatMethod.getName,
      asm.Type.getMethodDescriptor(stringConcatMethod), false)

    // Return the string
    method.visitInsn(ARETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 10)
    method.visitEnd()
  }

  /**
    * This method emits the code for `getBoxedValue()` method. This method returns an array of objects containing all the
    * elements of the tuple in the same order that they appear on the tuple but if the element is a primitive then it will
    * box the value.
    *
    * @param visitor   ClassWriter to emit method to the class
    * @param className Qualified name of the tuple class
    * @param fields    Fields of the class
    */
  private def compileGetBoxedValueMethod(visitor: ClassWriter, className: TupleClassName, fields: List[WrappedType]) = {
    // header of the method
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getBoxedValue", s"()[Ljava/lang/Object;", null, null)

    method.visitCode()

    // Creating an array of objected
    method.visitLdcInsn(fields.length)
    method.visitTypeInsn(ANEWARRAY, asm.Type.getInternalName(Constants.objectClass))

    // Putting boxed of value on the array
    fields.zipWithIndex.foreach { case (field, ind) =>
      // Duplicating the array address
      method.visitInsn(DUP)

      // Putting index on top of the stack
      method.visitLdcInsn(ind)

      // Boxing the field
      boxField(method, field, className, s"field$ind")

      // Storing the value inside the array
      method.visitInsn(AASTORE)
    }

    // Returning the array
    method.visitInsn(ARETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * This method search in the type for a tuple type
    *
    * @param tpe type to be searched
    * @return tuple types nested in the type
    */
  def findTuplesInTypes(tpe: Type): Set[Type] = {
    val base = tpe.typeConstructor
    val args = tpe.typeArguments
    if (base.isTuple) {
      Set(tpe) ++ args.flatMap(findTuplesInTypes)
    } else {
      args.flatMap(findTuplesInTypes).toSet
    }
  }

  /**
    * Find tuples from the given expression
    *
    * @param e expression
    * @return A list containing all the tuples in the given expression
    */
  def findTuplesInExps(e: Expression): Set[Type] = e match {
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
    case Expression.Var(sym, tpe, loc) => findTuplesInTypes(tpe)
    case Expression.Closure(ref, freeVars, _, tpe, loc) => findTuplesInTypes(tpe)
    case Expression.ApplyClo(exp, args, tpe, loc) => findTuplesInExps(exp) ++ args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.ApplyDef(name, args, tpe, loc) => args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.ApplyCloTail(exp, args, tpe, loc) => findTuplesInExps(exp) ++ args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.ApplyDefTail(name, args, tpe, loc) => args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => actuals.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.ApplyHook(hook, args, tpe, loc) => args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.Unary(sop, op, exp, tpe, loc) => findTuplesInExps(exp) ++ findTuplesInTypes(tpe)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => findTuplesInExps(exp1) ++ findTuplesInExps(exp2) ++ findTuplesInTypes(tpe)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => findTuplesInExps(exp1) ++ findTuplesInExps(exp2) ++ findTuplesInExps(exp3)
    case Expression.Branch(exp, branches, tpe, loc) => findTuplesInExps(exp) ++ branches.values.flatMap(findTuplesInExps).toSet
    case Expression.JumpTo(sym, tpe, loc) => Set.empty
    case Expression.Let(sym, exp1, exp2, tpe, loc) => findTuplesInExps(exp1) ++ findTuplesInExps(exp2) ++ findTuplesInTypes(tpe)
    case Expression.Is(sym, tag, exp, loc) => findTuplesInExps(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => findTuplesInExps(exp) ++ findTuplesInTypes(tpe)
    case Expression.Untag(sym, tag, exp, tpe, loc) => findTuplesInExps(exp) ++ findTuplesInTypes(tpe)
    case Expression.Index(base, offset, tpe, loc) => findTuplesInExps(base) ++ findTuplesInTypes(tpe)
    case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(findTuplesInTypes(tpe) + tpe)((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.Arr(elms, tpe, loc) => elms.foldLeft(findTuplesInTypes(tpe) + tpe)((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.ArrayLoad(base, index, tpe, loc) => findTuplesInExps(base) ++ findTuplesInExps(index) ++ findTuplesInTypes(tpe)
    case Expression.ArrayStore(base, index, value, tpe, loc) => findTuplesInExps(base) ++ findTuplesInExps(index) ++ findTuplesInExps(value) ++ findTuplesInTypes(tpe)
    case Expression.Ref(exp, tpe, loc) => findTuplesInExps(exp) ++ findTuplesInTypes(tpe)
    case Expression.Deref(exp, tpe, loc) => findTuplesInExps(exp) ++ findTuplesInTypes(tpe)
    case Expression.Assign(exp1, exp2, tpe, loc) => findTuplesInExps(exp1) ++ findTuplesInExps(exp2) ++ findTuplesInTypes(tpe)
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) => ??? // TODO
    case Expression.Existential(params, exp, loc) => findTuplesInExps(exp)
    case Expression.Universal(params, exp, loc) => findTuplesInExps(exp)
    case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.NativeField(field, tpe, loc) => findTuplesInTypes(tpe)
    case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(findTuplesInTypes(tpe))((acc, elem) => acc ++ findTuplesInExps(elem))
    case Expression.UserError(tpe, loc) => findTuplesInTypes(tpe)
    case Expression.HoleError(sym, tpe, loc) => findTuplesInTypes(tpe)
    case Expression.MatchError(tpe, loc) => findTuplesInTypes(tpe)
    case Expression.SwitchError(tpe, loc) => findTuplesInTypes(tpe)
  }
}
