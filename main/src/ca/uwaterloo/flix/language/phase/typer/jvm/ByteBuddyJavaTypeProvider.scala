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

import ca.uwaterloo.flix.language.ast.jvm.*
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError.{InvalidClass, MissingClass, UnsupportedDescriptor}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{ClassDescs, Result}
import net.bytebuddy.ClassFileVersion
import net.bytebuddy.description.TypeVariableSource
import net.bytebuddy.description.enumeration.EnumerationDescription
import net.bytebuddy.description.field.FieldDescription
import net.bytebuddy.description.method.MethodDescription
import net.bytebuddy.description.`type`.{TypeDefinition, TypeDescription}
import net.bytebuddy.dynamic.ClassFileLocator
import net.bytebuddy.dynamic.scaffold.MethodGraph
import net.bytebuddy.pool.TypePool

import java.lang.annotation.{Retention, RetentionPolicy}
import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.lang.reflect.{GenericSignatureFormatError, MalformedParameterizedTypeException}
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

object ByteBuddyJavaTypeProvider {

  /** Returns a provider for the classes visible through the JDK platform class loader. */
  def platform(): ByteBuddyJavaTypeProvider =
    fromLocators(List(ClassFileLocator.ForClassLoader.ofPlatformLoader()))

  /** Returns a provider that reads resources visible to `loader` without loading classes; `null` denotes the bootstrap loader. */
  def fromClassLoader(loader: ClassLoader): ByteBuddyJavaTypeProvider = {
    val locator = if (loader == null) {
      // The JVM represents the bootstrap class loader with null.
      ClassFileLocator.ForClassLoader.ofBootLoader()
    } else {
      // A non-null loader exposes application or user-provided class-path resources.
      ClassFileLocator.ForClassLoader.of(loader)
    }
    fromLocators(List(locator))
  }

  /** Returns a provider for JARs and class directories, with running-JVM multi-release entries and optional platform fallback. */
  def fromClassPath(entries: List[Path], includePlatform: Boolean = true): ByteBuddyJavaTypeProvider = {
    val version = ClassFileVersion.ofThisVm()
    val entryLocators = entries.map { path =>
      if (Files.isDirectory(path)) {
        ClassFileLocator.ForFolder.of(path.toFile, version)
      } else {
        ClassFileLocator.ForJarFile.of(path.toFile, version)
      }
    }
    val locators = if (includePlatform) {
      entryLocators :+ ClassFileLocator.ForClassLoader.ofPlatformLoader()
    } else {
      entryLocators
    }
    fromLocators(locators)
  }

  /** Returns a provider backed by the given locators in lookup order. */
  private def fromLocators(locators: List[ClassFileLocator]): ByteBuddyJavaTypeProvider = {
    val locator = new ClassFileLocator.Compound(locators.asJava)
    val pool = new TypePool.Default.WithLazyResolution(
      new TypePool.CacheProvider.Simple(),
      locator,
      TypePool.Default.ReaderMode.FAST
    )
    ByteBuddyJavaTypeProvider(locator, pool)
  }

}

/**
  * A [[JavaTypeProvider]] backed by Byte Buddy's lazy class-file type pool.
  *
  * Byte Buddy reads bytes through a [[ClassFileLocator]] and parses class-file metadata. It is never given a target
  * `Class`, and this implementation never calls `Class.forName` or `ClassLoader.loadClass`.
  */
final case class ByteBuddyJavaTypeProvider(
  locator: ClassFileLocator,
  pool: TypePool
) extends JavaTypeProvider {

  /** Caches converted class metadata for the lifetime of this provider. */
  private val classCache = new ConcurrentHashMap[ClassDesc, Result[JavaClass, JavaLookupError]]()

  /** Caches compiled virtual method graphs for the lifetime of this provider. */
  private val virtualMethodsCache = new ConcurrentHashMap[ClassDesc, Result[List[JavaMethod], JavaLookupError]]()

  /** Caches nominal subtype queries for the lifetime of this provider. */
  private val subtypeCache = new ConcurrentHashMap[(ClassDesc, ClassDesc), Result[Boolean, JavaLookupError]]()

  /** Returns `Ok` with metadata for `desc`, or `Err` if the descriptor is unsupported, missing, or invalid. */
  override def lookupClass(desc: ClassDesc): Result[JavaClass, JavaLookupError] =
    classCache.computeIfAbsent(desc, d => lookupClassDirect(d))

  /** Returns `Ok` with the virtual method graph for `desc`, or `Err` if the descriptor is unsupported, missing, or invalid. */
  override def virtualMethods(desc: ClassDesc): Result[List[JavaMethod], JavaLookupError] =
    virtualMethodsCache.computeIfAbsent(desc, d => virtualMethodsDirect(d))

  /** Returns `Ok` with the subtype result, or `Err` if either descriptor is unsupported, missing, or invalid. */
  override def isSubtype(subtype: ClassDesc, supertype: ClassDesc): Result[Boolean, JavaLookupError] =
    subtypeCache.computeIfAbsent((subtype, supertype), key => isSubtypeDirect(key._1, key._2))

  /** Closes the underlying class-file locator and its owned resources. */
  override def close(): Unit = locator.close()

  /** Computes [[lookupClass]] without consulting the cache. */
  private def lookupClassDirect(desc: ClassDesc): Result[JavaClass, JavaLookupError] =
    resolve(desc).map(toClass)

  /** Computes [[virtualMethods]] without consulting the cache. */
  private def virtualMethodsDirect(desc: ClassDesc): Result[List[JavaMethod], JavaLookupError] =
    resolve(desc).map { tpe =>
      MethodGraph.Compiler.Default.forJavaHierarchy().compile(tpe: TypeDefinition).listNodes().asScala
        .map(_.getRepresentative)
        .filter(_.isVirtual)
        .map(toMethod)
        .toList
        .sortBy(m => (m.ref.name, m.ref.descriptor.descriptorString()))
    }

  /** Computes [[isSubtype]] without consulting the cache. */
  private def isSubtypeDirect(subtype: ClassDesc, supertype: ClassDesc): Result[Boolean, JavaLookupError] = {
    if (subtype == supertype) {
      Ok(true)
    } else {
      resolve(subtype).flatMap { sub =>
        resolve(supertype).map(sup => sub.isAssignableTo(sup))
      }
    }
  }

  /** Returns `Ok` with the resolved type, or `Err` if `desc` is unsupported, missing, or invalid. */
  private def resolve(desc: ClassDesc): Result[TypeDescription, JavaLookupError] = {
    if (!desc.isClassOrInterface) {
      Err(UnsupportedDescriptor(desc))
    } else {
      try {
        val resolution = pool.describe(ClassDescs.binaryNameOf(desc))
        if (resolution.isResolved) Ok(resolution.resolve()) else Err(MissingClass(desc))
      } catch {
        case ex: GenericSignatureFormatError => Err(InvalidClass(desc, exceptionMessage(ex)))
        case ex: MalformedParameterizedTypeException => Err(InvalidClass(desc, exceptionMessage(ex)))
        case ex: IllegalArgumentException => Err(InvalidClass(desc, exceptionMessage(ex)))
        case ex: IllegalStateException => Err(InvalidClass(desc, exceptionMessage(ex)))
      }
    }
  }

  /** Converts a Byte Buddy type description to Java class-file metadata. */
  private def toClass(tpe: TypeDescription): JavaClass = {
    val desc = toClassDesc(tpe)
    val methods = tpe.getDeclaredMethods.asScala.toList

    JavaClass(
      desc = desc,
      modifiers = tpe.getModifiers,
      isRuntimeVisibleAnnotation = isRuntimeVisibleAnnotation(tpe),
      typeParameters = tpe.getTypeVariables.asScala.toList.map(toTypeParameter),
      superClass = Option(tpe.getSuperClass).map(toType),
      interfaces = tpe.getInterfaces.asScala.toList.map(toType),
      declaredConstructors = methods.filter(_.isConstructor).map(toMethod),
      declaredMethods = methods.filter(_.isMethod).map(toMethod),
      declaredFields = tpe.getDeclaredFields.asScala.toList.map(toField)
    )
  }

  /** Converts a Byte Buddy type definition to its erased class descriptor. */
  private def toClassDesc(tpe: TypeDefinition): ClassDesc =
    ClassDesc.ofDescriptor(tpe.asErasure().getDescriptor)

  /** Converts a Byte Buddy field description to Java field metadata. */
  private def toField(field: FieldDescription): JavaField = {
    val fieldType = toType(field.getType)
    val ref = JavaFieldRef(
      owner = toClassDesc(field.getDeclaringType.asErasure()),
      name = field.getName,
      descriptor = fieldType.erasure
    )
    JavaField(ref, field.getModifiers, fieldType)
  }

  /** Converts a Byte Buddy method description to Java method metadata. */
  private def toMethod(method: MethodDescription): JavaMethod = {
    val ref = toMethodRef(method)
    JavaMethod(
      ref = ref,
      modifiers = method.getModifiers,
      typeParameters = method.getTypeVariables.asScala.toList.map(toTypeParameter),
      parameterNames = method.getParameters.asDefined().asScala.toList.map(_.getName),
      parameterTypes = method.getParameters.asTypeList().asScala.toList.map(toType),
      returnType = toType(method.getReturnType),
      isConstructor = method.isConstructor,
      isVarArgs = method.isVarArgs
    )
  }

  /** Converts a Byte Buddy method description to its nominal class-file reference. */
  private def toMethodRef(method: MethodDescription): JavaMethodRef = {
    val defined = method.asDefined()
    JavaMethodRef(
      owner = toClassDesc(defined.getDeclaringType.asErasure()),
      name = defined.getInternalName,
      descriptor = MethodTypeDesc.ofDescriptor(defined.getDescriptor)
    )
  }

  /** Returns whether `tpe` is an annotation with runtime retention. */
  private def isRuntimeVisibleAnnotation(tpe: TypeDescription): Boolean = {
    if (!tpe.isAnnotation) {
      false
    } else {
      val retention = tpe.getDeclaredAnnotations.ofType(classOf[Retention])
      // The policy is read from the class file, so it is an enumeration description rather than a loaded enum constant.
      retention != null && retention.getValue("value").resolve(classOf[EnumerationDescription]).getValue == RetentionPolicy.RUNTIME.name()
    }
  }

  /** Converts a Byte Buddy generic type description to Java type metadata. */
  private def toType(tpe: TypeDescription.Generic): JavaType = {
    tpe.getSort match {
      case TypeDefinition.Sort.GENERIC_ARRAY =>
        JavaType.GenericArray(toType(tpe.getComponentType), toClassDesc(tpe.asErasure()))
      case TypeDefinition.Sort.NON_GENERIC => JavaType.NonGeneric(toClassDesc(tpe.asErasure()))
      case TypeDefinition.Sort.PARAMETERIZED =>
        JavaType.Parameterized(toClassDesc(tpe.asErasure()), tpe.getTypeArguments.asScala.toList.map(toType))
      case TypeDefinition.Sort.VARIABLE | TypeDefinition.Sort.VARIABLE_SYMBOLIC =>
        JavaType.Variable(toTypeVariable(tpe), toClassDesc(tpe.asErasure()))
      case TypeDefinition.Sort.WILDCARD =>
        val upperBounds = tpe.getUpperBounds.asScala.toList.map(toType)
        val lowerBounds = tpe.getLowerBounds.asScala.toList.map(toType)
        val erasure = upperBounds.headOption.map(_.erasure).getOrElse(ClassDesc.of("java.lang.Object"))
        JavaType.Wildcard(upperBounds, lowerBounds, erasure)
    }
  }

  /** Converts a Byte Buddy type variable declaration to a Java type parameter. */
  private def toTypeParameter(tpe: TypeDescription.Generic): JavaTypeParameter =
    JavaTypeParameter(toTypeVariable(tpe), tpe.getUpperBounds.asScala.toList.map(toType))

  /** Converts a Byte Buddy type variable to its owner-qualified Java identity. */
  private def toTypeVariable(tpe: TypeDescription.Generic): JavaTypeVariable = {
    val owner = tpe.getTypeVariableSource match {
      case clazz: TypeDescription => JavaTypeVariableOwner.Class(toClassDesc(clazz))
      case method: MethodDescription => JavaTypeVariableOwner.Method(toMethodRef(method))
      case _: TypeVariableSource => JavaTypeVariableOwner.Unknown
      case null => JavaTypeVariableOwner.Unknown
    }
    JavaTypeVariable(owner, tpe.getSymbol)
  }

  /** Returns the exception message, falling back to the exception class name when absent. */
  private def exceptionMessage(ex: Throwable): String =
    Option(ex.getMessage).getOrElse(ex.getClass.getName)

}
