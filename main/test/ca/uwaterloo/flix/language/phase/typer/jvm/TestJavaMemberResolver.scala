/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.jvm.{JavaMethod, JavaType, JavaTypeVariable, JavaTypeVariableOwner}
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaArgument.*
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError.MissingClass
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{ClassDescs, JvmUtils}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ConstantDescs.*
import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.lang.reflect.{Modifier, ParameterizedType, TypeVariable}
import scala.jdk.CollectionConverters.*

class TestJavaMemberResolver extends AnyFunSuite {

  test("constructors.SelectsExactMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_int))) match {
        case Ok(constructors) =>
          assert(constructors.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(I)V")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsReferenceSubtypeMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.ArrayList")
      val linkedList = ClassDesc.of("java.util.LinkedList")
      JavaMemberResolver.constructors(owner, List(Typed(linkedList))) match {
        case Ok(constructors) =>
          val collectionConstructor = MethodTypeDesc.ofDescriptor("(Ljava/util/Collection;)V")
          assert(constructors.map(_.ref.descriptor) == List(collectionConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsPrimitiveWideningMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.math.BigDecimal")
      JavaMemberResolver.constructors(owner, List(Typed(CD_byte))) match {
        case Ok(constructors) =>
          assert(constructors.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(I)V")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsPrimitiveBoxingMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.concurrent.atomic.AtomicReference")
      JavaMemberResolver.constructors(owner, List(Typed(CD_int))) match {
        case Ok(constructors) =>
          val objectConstructor = MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)V")
          assert(constructors.map(_.ref.descriptor) == List(objectConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.RejectsUnsupportedUnboxing") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_Integer))) match {
        case Ok(constructors) => assert(constructors.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReturnsTiedNullMatches") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Null)) match {
        case Ok(constructors) =>
          val expected = Set(
            MethodTypeDesc.ofDescriptor("(Ljava/lang/String;)V"),
            MethodTypeDesc.ofDescriptor("(Ljava/lang/CharSequence;)V")
          )
          assert(constructors.map(_.ref.descriptor).toSet == expected)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsFixedArityVarArgsMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.ProcessBuilder")
      val strings = CD_String.arrayType()
      JavaMemberResolver.constructors(owner, List(Typed(strings))) match {
        case Ok(constructors) =>
          val varArgsConstructor = MethodTypeDesc.ofDescriptor("([Ljava/lang/String;)V")
          assert(constructors.map(_.ref.descriptor) == List(varArgsConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsExpandedVarArgsMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.ProcessBuilder")
      val arguments = List(Typed(CD_String), Typed(CD_String))
      JavaMemberResolver.constructors(owner, arguments) match {
        case Ok(constructors) =>
          val varArgsConstructor = MethodTypeDesc.ofDescriptor("([Ljava/lang/String;)V")
          assert(constructors.map(_.ref.descriptor) == List(varArgsConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReturnsNoMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_boolean))) match {
        case Ok(constructors) => assert(constructors.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReportsMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(JavaMemberResolver.constructors(missing, Nil) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

  test("methods.SelectsExactInstanceOverload") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.ArrayList")
      JavaMemberResolver.methods(owner, "remove", List(Typed(CD_int)), static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(owner))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(I)Ljava/lang/Object;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.FallsBackToObject") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.List")
      JavaMemberResolver.methods(owner, "toString", Nil, static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(CD_Object))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("()Ljava/lang/String;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.ResolvesArrayObjectMethod") {
    implicit val flix: Flix = new Flix
    try {
      val owner = CD_String.arrayType()
      JavaMemberResolver.methods(owner, "toString", Nil, static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(CD_Object))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("()Ljava/lang/String;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.SelectsExpandedVarArgsMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.Arrays")
      val arguments = List(Typed(CD_String), Typed(CD_String))
      JavaMemberResolver.methods(owner, "asList", arguments, static = true) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(owner))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("([Ljava/lang/Object;)Ljava/util/List;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.ReportsMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(JavaMemberResolver.methods(missing, "method", Nil, static = false) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

  // --- overridableMethods ---

  /** Returns the overridable methods of `owner` named `name` with the given erased parameter descriptors. */
  private def overridable(owner: ClassDesc, name: String, parameters: ClassDesc*)(implicit flix: Flix): List[JavaMethod] =
    JavaMemberResolver.overridableMethods(owner) match {
      case Ok(methods) => methods.filter(m => m.ref.name == name && m.ref.descriptor.parameterList().asScala.toList == parameters.toList)
      case Err(error) => fail(error.toString)
    }

  /** Returns the single overridable method of `owner` named `name` with the given erased parameter descriptors. */
  private def theOverridable(owner: ClassDesc, name: String, parameters: ClassDesc*)(implicit flix: Flix): JavaMethod =
    overridable(owner, name, parameters*) match {
      case List(method) => method
      case methods => fail(s"Expected exactly one method, found: $methods")
    }

  /** Returns the type variable `name` of the class `owner`. */
  private def classVar(owner: ClassDesc, name: String): JavaType.Variable =
    JavaType.Variable(JavaTypeVariable(JavaTypeVariableOwner.Class(owner), name), CD_Object)

  test("overridableMethods.Direct.Comparator.compare") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.Comparator")
      val method = theOverridable(owner, "compare", CD_Object, CD_Object)
      assert(method.ref.owner == owner)
      assert(method.parameterTypes == List(classVar(owner, "T"), classVar(owner, "T")))
      assert(method.returnType == JavaType.NonGeneric(CD_int))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Direct.Callable.call") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.concurrent.Callable")
      val method = theOverridable(owner, "call")
      assert(method.returnType == classVar(owner, "V"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Direct.ArrayList.get") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.ArrayList")
      val method = theOverridable(owner, "get", CD_int)
      assert(method.ref.owner == owner)
      assert(method.returnType == classVar(owner, "E"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Direct.TreeMap.get") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.TreeMap")
      val method = theOverridable(owner, "get", CD_Object)
      assert(method.returnType == classVar(owner, "V"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Inherited.UnaryOperator.apply") {
    implicit val flix: Flix = new Flix
    try {
      // UnaryOperator<T> extends Function<T, T>, so both T and R of Function map to T of UnaryOperator.
      val owner = ClassDesc.of("java.util.function.UnaryOperator")
      val method = theOverridable(owner, "apply", CD_Object)
      assert(method.ref.owner == ClassDesc.of("java.util.function.Function"))
      assert(method.parameterTypes == List(classVar(owner, "T")))
      assert(method.returnType == classVar(owner, "T"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Inherited.BinaryOperator.apply") {
    implicit val flix: Flix = new Flix
    try {
      // BinaryOperator<T> extends BiFunction<T, T, T>.
      val owner = ClassDesc.of("java.util.function.BinaryOperator")
      val method = theOverridable(owner, "apply", CD_Object, CD_Object)
      assert(method.parameterTypes == List(classVar(owner, "T"), classVar(owner, "T")))
      assert(method.returnType == classVar(owner, "T"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.NoParams.Runnable.run") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.Runnable")
      val method = theOverridable(owner, "run")
      assert(method.parameterTypes == Nil)
      assert(method.returnType == JavaType.NonGeneric(CD_void))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.NoParams.Object.toString") {
    implicit val flix: Flix = new Flix
    try {
      val method = theOverridable(CD_Object, "toString")
      assert(method.ref.owner == CD_Object)
      assert(method.returnType == JavaType.NonGeneric(CD_String))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Static.ExcludesStaticMethods") {
    implicit val flix: Flix = new Flix
    try {
      assert(overridable(ClassDesc.of("java.lang.Integer"), "valueOf", CD_int).isEmpty)
      assert(overridable(ClassDesc.of("java.util.Collections"), "sort", ClassDesc.of("java.util.List")).isEmpty)
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.InheritedFromObject.Comparator.equals") {
    implicit val flix: Flix = new Flix
    try {
      // Comparator<T> redeclares equals(Object), so the declaration of Comparator is used.
      val owner = ClassDesc.of("java.util.Comparator")
      val method = theOverridable(owner, "equals", CD_Object)
      assert(method.ref.owner == owner)
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.InheritedFromObject.ArrayList.hashCode") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.ArrayList")
      val method = theOverridable(owner, "hashCode")
      assert(method.returnType == JavaType.NonGeneric(CD_int))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Renamed.TestGenericChildInterface.testMethod") {
    implicit val flix: Flix = new Flix
    try {
      // TestGenericChildInterface<T> extends TestGenericInterface<T1>, so T1 is expressed as T.
      val owner = ClassDesc.of("dev.flix.test.TestGenericChildInterface")
      val method = theOverridable(owner, "testMethod", CD_Object)
      assert(method.ref.owner == ClassDesc.of("dev.flix.test.TestGenericInterface"))
      assert(method.parameterTypes == List(classVar(owner, "T")))
      assert(method.returnType == classVar(owner, "T"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Renamed.TestGenericSubInterface.compareTo") {
    implicit val flix: Flix = new Flix
    try {
      // TestGenericSubInterface<T extends Comparable<T>> extends Comparable<T>.
      val owner = ClassDesc.of("dev.flix.test.TestGenericSubInterface")
      val method = theOverridable(owner, "compareTo", CD_Object)
      assert(method.ref.owner == ClassDesc.of("java.lang.Comparable"))
      // The bound of T is Comparable, so the variable erases to Comparable rather than Object.
      val comparable = ClassDesc.of("java.lang.Comparable")
      assert(method.parameterTypes == List(JavaType.Variable(JavaTypeVariable(JavaTypeVariableOwner.Class(owner), "T"), comparable)))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.TwoLevel.TestGenericGrandchildInterface") {
    implicit val flix: Flix = new Flix
    try {
      // Grandchild<U> -> TestGenericChildInterface<U> -> TestGenericInterface<T1>.
      val owner = ClassDesc.of("dev.flix.test.TestGenericGrandchildInterface")
      val testMethod = theOverridable(owner, "testMethod", CD_Object)
      assert(testMethod.ref.owner == ClassDesc.of("dev.flix.test.TestGenericInterface"))
      assert(testMethod.parameterTypes == List(classVar(owner, "U")))
      assert(testMethod.returnType == classVar(owner, "U"))

      val describe = theOverridable(owner, "describe")
      assert(describe.ref.owner == ClassDesc.of("dev.flix.test.TestGenericChildInterface"))
      assert(describe.returnType == JavaType.NonGeneric(CD_String))

      val identity = theOverridable(owner, "identity", CD_Object)
      assert(identity.ref.owner == owner)
      assert(identity.parameterTypes == List(classVar(owner, "U")))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Reordered.TestGenericSwappedInterface.apply") {
    implicit val flix: Flix = new Flix
    try {
      // TestGenericSwappedInterface<A, B> extends TestGenericInterface2<B, A>.
      val owner = ClassDesc.of("dev.flix.test.TestGenericSwappedInterface")
      val method = theOverridable(owner, "apply", CD_Object)
      assert(method.parameterTypes == List(classVar(owner, "B")))
      assert(method.returnType == classVar(owner, "A"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Interface.IncludesPublicObjectMethods") {
    implicit val flix: Flix = new Flix
    try {
      // An interface does not inherit from Object, but its implementations do.
      val owner = ClassDesc.of("java.lang.Runnable")
      assert(theOverridable(owner, "toString").ref.owner == CD_Object)
      assert(theOverridable(owner, "equals", CD_Object).ref.owner == CD_Object)
      assert(theOverridable(owner, "hashCode").ref.owner == CD_Object)
      // Final and protected methods of Object are not overridable through an interface.
      assert(overridable(owner, "getClass").isEmpty)
      assert(overridable(owner, "clone").isEmpty)
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Class.IncludesProtectedMethods") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("dev.flix.test.TestClassWithProtectedMethods")
      val abstractMethod = theOverridable(owner, "protectedAbstractMethod", CD_int)
      assert(Modifier.isProtected(abstractMethod.modifiers) && Modifier.isAbstract(abstractMethod.modifiers))
      assert(Modifier.isProtected(theOverridable(owner, "protectedConcreteMethod", CD_String).modifiers))
      // Protected methods inherited from Object are overridable by a subclass.
      assert(theOverridable(owner, "clone").ref.owner == CD_Object)
      assert(theOverridable(ClassDesc.of("java.util.AbstractList"), "removeRange", CD_int, CD_int).ref.owner == ClassDesc.of("java.util.AbstractList"))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.Class.ExcludesFinalMethods") {
    implicit val flix: Flix = new Flix
    try {
      assert(overridable(ClassDesc.of("java.util.ArrayList"), "getClass").isEmpty)
      assert(overridable(ClassDesc.of("java.util.ArrayList"), "wait").isEmpty)
      assert(overridable(ClassDesc.of("java.lang.Thread"), "join").isEmpty)
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.ReportsMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(JavaMemberResolver.overridableMethods(missing) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

  test("overridableMethods.AgreesWithReflection") {
    implicit val flix: Flix = new Flix
    try {
      val classes = List(
        classOf[Object], classOf[Runnable], classOf[Comparable[?]], classOf[CharSequence], classOf[Number],
        classOf[Exception], classOf[RuntimeException], classOf[Thread], classOf[ClassLoader],
        classOf[java.io.InputStream], classOf[java.io.OutputStream], classOf[java.io.Reader],
        classOf[java.util.Comparator[?]], classOf[java.util.concurrent.Callable[?]], classOf[java.util.TimerTask],
        classOf[java.lang.Iterable[?]], classOf[java.util.Iterator[?]], classOf[java.util.Collection[?]],
        classOf[java.util.List[?]], classOf[java.util.Map[?, ?]], classOf[java.util.Map.Entry[?, ?]],
        classOf[java.util.AbstractCollection[?]], classOf[java.util.AbstractList[?]], classOf[java.util.AbstractMap[?, ?]],
        classOf[java.util.ArrayList[?]], classOf[java.util.LinkedList[?]], classOf[java.util.HashMap[?, ?]], classOf[java.util.TreeMap[?, ?]],
        classOf[java.util.function.Function[?, ?]], classOf[java.util.function.BiFunction[?, ?, ?]],
        classOf[java.util.function.UnaryOperator[?]], classOf[java.util.function.BinaryOperator[?]],
        classOf[java.util.function.Supplier[?]], classOf[java.util.function.Consumer[?]], classOf[java.util.function.Predicate[?]],
        classOf[dev.flix.test.TestBoundedGenericInterface[?]], classOf[dev.flix.test.TestClass],
        classOf[dev.flix.test.TestClassWithInheritedMethod], classOf[dev.flix.test.TestClassWithProtectedMethods],
        classOf[dev.flix.test.TestDefaultMethods], classOf[dev.flix.test.TestFunctionalInterface],
        classOf[dev.flix.test.TestGenericAbstractClass[?]], classOf[dev.flix.test.TestGenericAbstractClass2[?, ?]],
        classOf[dev.flix.test.TestGenericChildInterface[?]], classOf[dev.flix.test.TestGenericDefaultMethods[?]],
        classOf[dev.flix.test.TestGenericGrandchildInterface[?]], classOf[dev.flix.test.TestGenericInterface[?]],
        classOf[dev.flix.test.TestGenericInterface2[?, ?]], classOf[dev.flix.test.TestGenericInterface3[?, ?, ?]],
        classOf[dev.flix.test.TestGenericMethod], classOf[dev.flix.test.TestGenericSubInterface[?]],
        classOf[dev.flix.test.TestGenericSwappedInterface[?, ?]], classOf[dev.flix.test.TestOverloadedMethods],
        classOf[dev.flix.test.TestThrowingInterface], classOf[dev.flix.test.TestVarargsInterface], classOf[dev.flix.test.TestVoidInterface]
      )
      for (clazz <- classes) {
        val owner = ClassDescs.of(clazz)
        val expected = reflectiveSignatures(clazz)
        val actual = JavaMemberResolver.overridableMethods(owner) match {
          case Ok(methods) => methods.map(descriptorSignature(owner, _)).toSet
          case Err(error) => fail(error.toString)
        }
        assert(actual == expected, s"for ${clazz.getName}: missing ${expected -- actual}, extra ${actual -- expected}")
      }
    } finally flix.javaTypeProvider.close()
  }

  /**
    * Returns the canonical signatures of the overridable methods of `clazz` computed by reflection.
    *
    * Final and synthetic methods are dropped since `overridableMethods` excludes them by design.
    */
  private def reflectiveSignatures(clazz: Class[?]): Set[String] = {
    JvmUtils.getOverridableInstanceMethods(clazz).filterNot(m => Modifier.isFinal(m.getModifiers) || m.isSynthetic).map { method =>
      val mapping = JvmUtils.resolveTypeParamMapping(method, clazz)
      val parameters = method.getGenericParameterTypes.toList.map(reflectiveType(_, mapping))
      val returnType = reflectiveType(method.getGenericReturnType, mapping)
      s"${method.getName}(${parameters.mkString(", ")}): $returnType"
    }.toSet
  }

  /** Renders a reflective Java type the way Safety resolves it: class type variables by index, everything else erased to Object. */
  private def reflectiveType(tpe: java.lang.reflect.Type, mapping: Map[String, Int]): String = tpe match {
    case tv: TypeVariable[?] => mapping.get(tv.getName).map(i => s"#$i").getOrElse("java.lang.Object")
    case pt: ParameterizedType => pt.getRawType match {
      case raw: Class[?] => s"${raw.getName}<${pt.getActualTypeArguments.toList.map(reflectiveType(_, mapping)).mkString(", ")}>"
      case _ => "java.lang.Object"
    }
    case c: Class[?] => c.getName
    case _ => "java.lang.Object"
  }

  /** Renders the signature of a descriptor-based `method` of `owner` in the same canonical form as [[reflectiveSignatures]]. */
  private def descriptorSignature(owner: ClassDesc, method: JavaMethod)(implicit flix: Flix): String = {
    val typeParameters = flix.javaTypeProvider.lookupClass(owner).toOption.get.typeParameters.map(_.variable)
    def render(tpe: JavaType): String = tpe match {
      case JavaType.Variable(variable, _) => typeParameters.indexOf(variable) match {
        case -1 => "java.lang.Object"
        case i => s"#$i"
      }
      case JavaType.Parameterized(erasure, arguments) => s"${ClassDescs.binaryNameOf(erasure)}<${arguments.map(render).mkString(", ")}>"
      case JavaType.NonGeneric(erasure) => ClassDescs.binaryNameOf(erasure)
      case _ => "java.lang.Object"
    }
    s"${method.ref.name}(${method.parameterTypes.map(render).mkString(", ")}): ${render(method.returnType)}"
  }

}
