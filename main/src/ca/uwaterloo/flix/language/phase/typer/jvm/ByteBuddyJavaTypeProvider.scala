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

import ca.uwaterloo.flix.language.ast.jvm.{JavaClass, JavaField, JavaFieldRef, JavaMethod, JavaMethodRef, JavaType, JavaTypeParameter, JavaTypeVariable, JavaTypeVariableOwner}
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError.{InvalidClass, MissingClass, UnsupportedDescriptor}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import net.bytebuddy.ClassFileVersion
import net.bytebuddy.description.TypeVariableSource
import net.bytebuddy.description.field.FieldDescription
import net.bytebuddy.description.method.{MethodDescription, ParameterDescription}
import net.bytebuddy.description.`type`.{TypeDefinition, TypeDescription}
import net.bytebuddy.dynamic.ClassFileLocator
import net.bytebuddy.pool.TypePool

import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * A [[JavaTypeProvider]] backed by Byte Buddy's lazy class-file type pool.
  *
  * Byte Buddy reads bytes through a [[ClassFileLocator]] and parses class-file metadata. It is never given a target
  * `Class`, and this implementation never calls `Class.forName` or `ClassLoader.loadClass`.
  */
final class ByteBuddyJavaTypeProvider private(
  private val locator: ClassFileLocator,
  private val pool: TypePool
) extends JavaTypeProvider {

  override def lookupClass(desc: ClassDesc): Result[JavaClass, JavaLookupError] =
    resolve(desc).flatMap(tpe => attempt(desc)(toClass(tpe)))

  override def close(): Unit = locator.close()

  private def resolve(desc: ClassDesc): Result[TypeDescription, JavaLookupError] = {
    binaryName(desc) match {
      case None => Err(UnsupportedDescriptor(desc))
      case Some(name) =>
        attempt(desc)(pool.describe(name)).flatMap { resolution =>
          if (resolution.isResolved) attempt(desc)(resolution.resolve()) else Err(MissingClass(desc))
        }
    }
  }

  private def toClass(tpe: TypeDescription): JavaClass = {
    val desc = toClassDesc(tpe)
    val methods = tpe.getDeclaredMethods.asScala.toList

    JavaClass(
      desc = desc,
      modifiers = tpe.getModifiers,
      typeParameters = tpe.getTypeVariables.asScala.toList.map(toTypeParameter),
      superClass = Option(tpe.getSuperClass).map(toType),
      interfaces = tpe.getInterfaces.asScala.toList.map(toType),
      declaredConstructors = methods.filter(_.isConstructor).map(toMethod),
      declaredMethods = methods.filter(_.isMethod).map(toMethod),
      declaredFields = tpe.getDeclaredFields.asScala.toList.map(toField)
    )
  }

  private def toMethod(method: MethodDescription): JavaMethod = {
    val ref = toMethodRef(method)
    JavaMethod(
      ref = ref,
      modifiers = method.getModifiers,
      typeParameters = method.getTypeVariables.asScala.toList.map(toTypeParameter),
      parameterTypes = method.getParameters.asScala.toList
        .map(_.asInstanceOf[ParameterDescription])
        .map(p => toType(p.getType)),
      returnType = toType(method.getReturnType),
      isConstructor = method.isConstructor,
      isVarArgs = method.isVarArgs
    )
  }

  private def toField(field: FieldDescription): JavaField = {
    val fieldType = toType(field.getType)
    val ref = JavaFieldRef(
      owner = toClassDesc(field.getDeclaringType.asErasure()),
      name = field.getName,
      descriptor = fieldType.erasure
    )
    JavaField(ref, field.getModifiers, fieldType)
  }

  private def toTypeParameter(tpe: TypeDescription.Generic): JavaTypeParameter =
    JavaTypeParameter(toTypeVariable(tpe), tpe.getUpperBounds.asScala.toList.map(toType))

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

  private def toTypeVariable(tpe: TypeDescription.Generic): JavaTypeVariable = {
    val owner = tpe.getTypeVariableSource match {
      case clazz: TypeDescription => JavaTypeVariableOwner.Class(toClassDesc(clazz))
      case method: MethodDescription => JavaTypeVariableOwner.Method(toMethodRef(method))
      case _: TypeVariableSource => JavaTypeVariableOwner.Unknown
      case null => JavaTypeVariableOwner.Unknown
    }
    JavaTypeVariable(owner, tpe.getSymbol)
  }

  private def toMethodRef(method: MethodDescription): JavaMethodRef = {
    // Preserve the method's defined class-file descriptor in its nominal identity while retaining generic parameter
    // and return types in JavaMethod.
    val defined = method.asDefined()
    JavaMethodRef(
      owner = toClassDesc(defined.getDeclaringType.asErasure()),
      name = defined.getInternalName,
      descriptor = MethodTypeDesc.ofDescriptor(defined.getDescriptor)
    )
  }

  private def toClassDesc(tpe: TypeDefinition): ClassDesc =
    ClassDesc.ofDescriptor(tpe.asErasure().getDescriptor)

  private def binaryName(desc: ClassDesc): Option[String] = {
    val descriptor = desc.descriptorString()
    if (descriptor.startsWith("L") && descriptor.endsWith(";")) {
      Some(descriptor.substring(1, descriptor.length - 1).replace('/', '.'))
    } else {
      None
    }
  }

  private def attempt[A](desc: ClassDesc)(f: => A): Result[A, JavaLookupError] =
    try Ok(f)
    catch {
      case NonFatal(ex) => Err(InvalidClass(desc, Option(ex.getMessage).getOrElse(ex.getClass.getName)))
    }

}

object ByteBuddyJavaTypeProvider {

  /** Returns a provider for the classes visible through the JDK platform class loader. */
  def platform(): ByteBuddyJavaTypeProvider =
    fromLocators(List(ClassFileLocator.ForClassLoader.ofPlatformLoader()))

  /**
    * Returns a provider that reads resources visible to `loader`.
    *
    * Resource lookup does not define or initialize the class being described. A `null` loader denotes the bootstrap
    * class loader.
    */
  def fromClassLoader(loader: ClassLoader): ByteBuddyJavaTypeProvider = {
    val locator = if (loader == null) ClassFileLocator.ForClassLoader.ofBootLoader() else ClassFileLocator.ForClassLoader.of(loader)
    fromLocators(List(locator))
  }

  /**
    * Returns a provider for explicit JARs and class directories.
    *
    * Multi-release entries are selected for the running JVM. Platform classes are appended as a fallback when
    * `includePlatform` is `true`.
    */
  def fromClassPath(entries: List[Path], includePlatform: Boolean = true): ByteBuddyJavaTypeProvider = {
    val version = ClassFileVersion.ofThisVm()
    val entryLocators = entries.map { path =>
      if (Files.isDirectory(path)) ClassFileLocator.ForFolder.of(path.toFile, version)
      else ClassFileLocator.ForJarFile.of(path.toFile, version)
    }
    val locators = if (includePlatform) entryLocators :+ ClassFileLocator.ForClassLoader.ofPlatformLoader() else entryLocators
    fromLocators(locators)
  }

  /** Constructs a provider from an arbitrary locator. Intended for focused tests. */
  private[jvm] def fromLocator(locator: ClassFileLocator): ByteBuddyJavaTypeProvider =
    fromLocators(List(locator))

  private def fromLocators(locators: List[ClassFileLocator]): ByteBuddyJavaTypeProvider = {
    val locator = new ClassFileLocator.Compound(locators.asJava)
    val pool = new TypePool.Default.WithLazyResolution(
      new TypePool.CacheProvider.Simple(),
      locator,
      TypePool.Default.ReaderMode.FAST
    )
    new ByteBuddyJavaTypeProvider(locator, pool)
  }

}
