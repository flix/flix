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
import ca.uwaterloo.flix.language.ast.jvm.{JavaField, JavaMethod}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.Ok

import java.lang.constant.ConstantDescs.*
import java.lang.constant.ClassDesc
import java.lang.reflect.Modifier
import scala.jdk.CollectionConverters.*

/*
 * Java executable overload resolution intentionally mirrors Apache Commons
 * Lang's matching constructor and method lookup and the transformation costs
 * in `MemberUtils`. It is a compatibility algorithm for Flix's existing
 * reflective lookup, not an implementation of JLS most-specific resolution.
 *
 * Resolution separates legality from preference. A candidate must first be
 * applicable; only applicable candidates receive a cost. Consequently, a low
 * cost can never make an illegal argument conversion acceptable.
 *
 * Exact erased descriptor matches are selected before any scoring. This covers
 * ordinary exact parameters and passing an array directly to a varargs
 * executable. A `Null` argument has no descriptor and cannot be exact.
 *
 * Applicability supports the conversions recognized by the current reflective
 * path: widening references, widening primitives, boxing, unboxing, `null` to
 * references, Java array subtyping, and both fixed and expanded varargs. This
 * step only answers whether an executable can participate in ranking.
 *
 * Each applicable executable then receives one cost per argument. The costs
 * are added, producing a single number for the complete parameter list. A
 * candidate may therefore compensate for a worse match at one parameter with
 * a better match at another parameter.
 *
 * An exact argument-to-parameter type costs zero. For class parameters, each
 * superclass step costs 1.0. An assignable interface costs 0.25, making it
 * worse than an exact match but better than one superclass step. The interface
 * hierarchy depth is deliberately not counted.
 *
 * A `Null` argument costs 1.5 for every reference parameter. Thus `String` and
 * `CharSequence` receive equal costs for `null`, even though JLS overload
 * resolution would consider `String` more specific.
 *
 * Primitive promotion follows Commons Lang's ordered list:
 *
 *   byte, short, char, int, long, float, double
 *
 * Moving forward by one position costs 0.1. Applicability is checked first, so
 * this heuristic ordering cannot legalize an invalid primitive conversion.
 * Unwrapping a wrapper before primitive promotion adds another 0.1.
 *
 * Varargs receive a small 0.001 penalty. When an explicit array is supplied,
 * its component type is compared with the declared component type. For
 * expanded varargs, every trailing argument is compared with the component
 * type and receives the penalty. When no varargs values are supplied, Commons
 * Lang gives the more generic component type the lower cost.
 *
 * After scoring, every candidate with the minimum total cost is returned.
 * Commons Lang keeps the first minimum encountered in its deterministic method
 * order or in reflection's unspecified constructor order. Returning every tied
 * minimum lets shadow comparison accept any semantically equivalent Commons
 * Lang choice without depending on metadata order.
 *
 * Wrapper-to-primitive unboxing is considered during applicability and scoring
 * because Commons Lang considers it. Flix does not support that conversion in
 * lowering, so tied best candidates requiring it are removed only after best
 * candidate selection. Filtering earlier could select a worse supported
 * executable, which would not match the existing reflective path.
 *
 * The score should therefore be read as an estimate of conversion distance
 * used to preserve existing behavior. It is not a runtime conversion, a proof
 * of type safety, or a general model of Java overload resolution.
 */
object JavaMemberResolver {

  import JavaArgument.*

  /** The penalty added for each varargs conversion. */
  private val VarArgsCost = 0.001f

  /** The class-file modifier bit that identifies a compiler-generated bridge method. */
  private val BridgeModifier = 0x0040

  /** The class-file modifier bit that identifies a compiler-generated synthetic member. */
  private val SyntheticModifier = 0x1000

  /** The descriptor of `java.lang.Cloneable`, a direct supertype of every array type. */
  private val CloneableDesc = ClassDesc.of("java.lang.Cloneable")

  /** The descriptor of `java.io.Serializable`, a direct supertype of every array type. */
  private val SerializableDesc = ClassDesc.of("java.io.Serializable")

  /** The primitive types in the promotion order used by Commons Lang. */
  private val WideningPrimitives = List(CD_byte, CD_short, CD_char, CD_int, CD_long, CD_float, CD_double)

  /** Maps each non-void primitive descriptor to its wrapper descriptor. */
  private val PrimitiveToWrapper = Map(
    CD_boolean -> CD_Boolean,
    CD_byte -> CD_Byte,
    CD_short -> CD_Short,
    CD_char -> CD_Character,
    CD_int -> CD_Integer,
    CD_long -> CD_Long,
    CD_float -> CD_Float,
    CD_double -> CD_Double
  )

  /** Maps each primitive wrapper descriptor to its primitive descriptor. */
  private val WrapperToPrimitive = PrimitiveToWrapper.map {
    case (primitive, wrapper) => wrapper -> primitive
  }

  /** Returns `Ok` with every tied best public constructor, or `Err` if class metadata cannot be read. */
  def constructors(owner: ClassDesc, arguments: List[JavaArgument])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
      val candidates = clazz.declaredConstructors.filter(method => Modifier.isPublic(method.modifiers))
      best(candidates, arguments).map(_.filterNot(usesUnsupportedUnboxing(arguments, _)))
    }

  /** Returns `Ok` with every tied best public method, including the existing `Object` fallback. */
  def methods(owner: ClassDesc, name: String, arguments: List[JavaArgument], static: Boolean)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    resolveMethods(owner, name, arguments, static).flatMap {
      case result if result.nonEmpty || owner == CD_Object => Ok(result)
      case _ => resolveMethods(CD_Object, name, arguments, static)
    }

  /** Returns `Ok` with the selected public field, or `Err` if class metadata cannot be read. */
  def field(owner: ClassDesc, name: String, static: Boolean)(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] = {
    if (owner.isClassOrInterface) {
      findField(owner, name, Set.empty).map(_.filter(f => Modifier.isStatic(f.modifiers) == static))
    } else {
      Ok(None)
    }
  }

  /**
    * Returns `Ok` with the instance methods of `owner` that an anonymous subclass may override, or `Err` if class
    * metadata cannot be read.
    *
    * The result holds the most-derived declaration of every `public` or `protected` instance method of `owner`,
    * including inherited methods and interface default methods, sorted by name and descriptor. Since an interface
    * does not inherit from `Object`, the public methods of `Object` are added for an interface unless it declares
    * a method with the same erased signature.
    *
    * `static`, `final`, `private`, and package-private methods are excluded since a subclass in another package
    * cannot override them. Synthetic and bridge methods are excluded since they are not source-level members.
    *
    * The parameter and return types of every method refer to the type parameters of `owner`, even if the method is
    * declared by a supertype. For example, `UnaryOperator<T>` inherits `apply` from `Function<T, R>` and the returned
    * method has type `(T) -> T` where `T` is the type parameter of `UnaryOperator`.
    */
  def overridableMethods(owner: ClassDesc)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    virtualMethodsWhere(owner, isOverridable)

  /**
    * Returns `Ok` with the public instance methods of `owner`, including inherited methods, or `Err` if class
    * metadata cannot be read.
    *
    * An array exposes the methods of `Object` and a primitive type exposes no methods. Since an interface does not
    * inherit from `Object`, the public methods of `Object` are added for an interface unless it declares a method
    * with the same erased signature. Bridge and synthetic methods are excluded.
    */
  def instanceMethods(owner: ClassDesc)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = {
    if (owner.isArray) instanceMethods(CD_Object)
    else if (owner.isPrimitive) Ok(Nil)
    else virtualMethodsWhere(owner, isPublicInstance)
  }

  /**
    * Returns `Ok` with the public static methods of `owner`, including those inherited from superclasses, or `Err`
    * if class metadata cannot be read.
    *
    * Static methods of superinterfaces are not inherited, and a static method hides an inherited static method with
    * the same erased signature.
    */
  def staticMethods(owner: ClassDesc)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = {
    if (!owner.isClassOrInterface) Ok(Nil)
    else staticMethodsWhere(owner, _ => true, Set.empty).map(_.sortBy(method => (method.ref.name, method.ref.descriptor.descriptorString())))
  }

  /**
    * Returns `Ok` with the public fields of `owner`, including those inherited from superclasses and superinterfaces,
    * sorted by name, or `Err` if class metadata cannot be read.
    */
  def fields(owner: ClassDesc)(implicit flix: Flix): Result[List[JavaField], JavaLookupError] = {
    if (!owner.isClassOrInterface) Ok(Nil)
    else collectFields(owner, Set.empty).map(_.distinctBy(_.ref).sortBy(_.ref.name))
  }

  /**
    * Returns the virtual methods of `owner` that satisfy `keep`, sorted by name and descriptor.
    *
    * Since an interface does not inherit from `Object`, the public methods of `Object` that satisfy `keep` are added
    * for an interface unless it declares a method with the same erased signature.
    */
  private def virtualMethodsWhere(owner: ClassDesc, keep: JavaMethod => Boolean)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
      flix.javaTypeProvider.virtualMethods(owner).flatMap { virtualMethods =>
        val declared = virtualMethods.filter(keep)
        if (!Modifier.isInterface(clazz.modifiers)) {
          Ok(declared)
        } else {
          // The methods of Object are inherited by every implementation of the interface.
          flix.javaTypeProvider.virtualMethods(CD_Object).map { objectMethods =>
            val declaredKeys = virtualMethods.map(methodKey).toSet
            val inherited = objectMethods.filter { method =>
              Modifier.isPublic(method.modifiers) && keep(method) && !declaredKeys.contains(methodKey(method))
            }
            (declared ::: inherited).sortBy(method => (method.ref.name, method.ref.descriptor.descriptorString()))
          }
        }
      }
    }

  /** Returns the public fields declared by `owner` and its supertypes, in declaration order. */
  private def collectFields(owner: ClassDesc, visited: Set[ClassDesc])(implicit flix: Flix): Result[List[JavaField], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(Nil)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        val declared = clazz.declaredFields.filter(field => Modifier.isPublic(field.modifiers) && !isSynthetic(field.modifiers))
        val parents = clazz.interfaces.map(_.erasure) ::: clazz.superClass.map(_.erasure).toList
        Result.traverse(parents)(parent => collectFields(parent, visited + owner)).map(inherited => declared ::: inherited.flatten)
      }
    }
  }

  /** Selects the best supported methods from `owner`, then retains the requested kind. */
  private def resolveMethods(owner: ClassDesc, name: String, arguments: List[JavaArgument], static: Boolean)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    rawMethodCandidates(owner, name).flatMap { candidates =>
      // Commons Lang returns an exact Class.getMethod result before declaring-owner normalization.
      val exact = exactMatches(candidates, arguments)
      val selected = if (exact.nonEmpty) Ok(exact) else normalizeAccessibleMethods(candidates).flatMap(best(_, arguments))
      selected.map { methods =>
        // The existing TypeReduction path checks the static flag only after overload selection.
        methods.filter(method => Modifier.isStatic(method.modifiers) == static)
          .filterNot(usesUnsupportedUnboxing(arguments, _))
      }
    }

  /** Returns public candidates before declaring-owner accessibility normalization. */
  private def rawMethodCandidates(owner: ClassDesc, name: String)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = {
    if (owner.isArray) {
      rawMethodCandidates(CD_Object, name)
    } else if (owner.isPrimitive) {
      Ok(Nil)
    } else {
      virtualMethodCandidates(owner, name).flatMap { virtualMethods =>
        staticMethodCandidates(owner, name, Set.empty).map(staticMethods => (virtualMethods ::: staticMethods).distinctBy(_.ref))
      }
    }
  }

  /** Returns public virtual graph representatives together with visible synthetic bridges. */
  private def virtualMethodCandidates(owner: ClassDesc, name: String)(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = {
    flix.javaTypeProvider.virtualMethods(owner).flatMap { virtualMethods =>
      bridgeMethods(owner, name, Set.empty, Set.empty).map { bridges =>
        val methods = virtualMethods.filter { method =>
          method.ref.name == name && Modifier.isPublic(method.modifiers) && !Modifier.isStatic(method.modifiers)
        }
        val bridgeDescriptors = bridges.map(method => method.ref.name -> method.ref.descriptor).toSet
        val visibleMethods = methods.filterNot(method => bridgeDescriptors.contains(method.ref.name -> method.ref.descriptor))
        (visibleMethods ::: bridges).distinctBy(_.ref)
      }
    }
  }

  /** Returns visible public bridge methods from the class and interface hierarchy. */
  private def bridgeMethods(owner: ClassDesc,
                            name: String,
                            hidden: Set[(String, List[ClassDesc])],
                            visited: Set[ClassDesc])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(Nil)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        val declarations = clazz.declaredMethods.filter { method =>
          !Modifier.isStatic(method.modifiers) && !Modifier.isPrivate(method.modifiers)
        }
        val bridges = declarations.filter { method =>
          method.ref.name == name && Modifier.isPublic(method.modifiers) && isBridge(method) && !hidden.contains(methodKey(method))
        }
        val nextHidden = hidden ++ declarations.map(methodKey)
        val parents = clazz.superClass.map(_.erasure).toList ::: clazz.interfaces.map(_.erasure)
        Result.traverse(parents)(parent => bridgeMethods(parent, name, nextHidden, visited + owner))
          .map(inherited => bridges ::: inherited.flatten)
      }
    }
  }

  /** Returns public static methods named `name`, applying Java superclass inheritance and hiding. */
  private def staticMethodCandidates(owner: ClassDesc,
                                     name: String,
                                     visited: Set[ClassDesc])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    staticMethodsWhere(owner, _.ref.name == name, visited)

  /** Returns public static methods satisfying `keep`, applying Java superclass inheritance and hiding. */
  private def staticMethodsWhere(owner: ClassDesc,
                                 keep: JavaMethod => Boolean,
                                 visited: Set[ClassDesc])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(Nil)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        val staticMethods = clazz.declaredMethods.filter { method =>
          keep(method) && Modifier.isStatic(method.modifiers) && !isSynthetic(method.modifiers)
        }
        val declared = staticMethods.filter(method => Modifier.isPublic(method.modifiers))
        if (Modifier.isInterface(clazz.modifiers)) {
          // Static interface methods are not inherited from superinterfaces.
          Ok(declared)
        } else clazz.superClass match {
          case None => Ok(declared)
          case Some(parent) =>
            staticMethodsWhere(parent.erasure, keep, visited + owner).map { inherited =>
              val hidden = staticMethods.map(methodKey).toSet
              declared ::: inherited.filterNot(method => hidden.contains(methodKey(method)))
            }
        }
      }
    }
  }

  /** Replaces methods on non-public declaring classes with accessible interface or superclass declarations. */
  private def normalizeAccessibleMethods(methods: List[JavaMethod])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    Result.traverse(methods)(accessibleMethod).map(_.flatten.distinctBy(_.ref))

  /** Returns the accessible declaration corresponding to `method`, if one exists. */
  private def accessibleMethod(method: JavaMethod)(implicit flix: Flix): Result[Option[JavaMethod], JavaLookupError] =
    flix.javaTypeProvider.lookupClass(method.ref.owner).flatMap { owner =>
      if (!Modifier.isPublic(method.modifiers)) {
        Ok(None)
      } else if (Modifier.isPublic(owner.modifiers)) {
        Ok(Some(method))
      } else {
        findAccessibleInterfaceMethod(method.ref.owner, method, Set.empty).flatMap {
          case result@Some(_) => Ok(result)
          case None => findAccessibleSuperclassMethod(method.ref.owner, method, Set.empty)
        }
      }
    }

  /** Finds the first matching declaration in the public interface nest of `owner`. */
  private def findAccessibleInterfaceMethod(owner: ClassDesc,
                                            method: JavaMethod,
                                            visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaMethod], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(None)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        findAccessibleInterfaceMethodIn(clazz.interfaces.map(_.erasure), method, visited + owner).flatMap {
          case result@Some(_) => Ok(result)
          case None => clazz.superClass match {
            case None => Ok(None)
            case Some(parent) => findAccessibleInterfaceMethod(parent.erasure, method, visited + owner)
          }
        }
      }
    }
  }

  /** Finds the first exact method in the public interfaces in declaration order. */
  private def findAccessibleInterfaceMethodIn(owners: List[ClassDesc],
                                              method: JavaMethod,
                                              visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaMethod], JavaLookupError] = owners match {
    case Nil => Ok(None)
    case owner :: rest if visited.contains(owner) => findAccessibleInterfaceMethodIn(rest, method, visited)
    case owner :: rest =>
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        if (!Modifier.isPublic(clazz.modifiers)) {
          findAccessibleInterfaceMethodIn(rest, method, visited)
        } else {
          clazz.declaredMethods.find(candidate => Modifier.isPublic(candidate.modifiers) && sameErasedSignature(candidate, method)) match {
            case result@Some(_) => Ok(result)
            case None =>
              findAccessibleInterfaceMethodIn(clazz.interfaces.map(_.erasure), method, visited + owner).flatMap {
                case result@Some(_) => Ok(result)
                case None => findAccessibleInterfaceMethodIn(rest, method, visited + owner)
              }
          }
        }
      }
  }

  /** Finds the matching method exposed by the first public superclass of `owner`. */
  private def findAccessibleSuperclassMethod(owner: ClassDesc,
                                             method: JavaMethod,
                                             visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaMethod], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(None)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        clazz.superClass match {
          case None => Ok(None)
          case Some(parent) =>
            flix.javaTypeProvider.lookupClass(parent.erasure).flatMap { parentClass =>
              if (Modifier.isPublic(parentClass.modifiers)) {
                rawMethodCandidates(parent.erasure, method.ref.name)
                  .map(_.find(candidate => sameErasedSignature(candidate, method)))
              } else {
                findAccessibleSuperclassMethod(parent.erasure, method, visited + owner)
              }
            }
        }
      }
    }
  }

  /**
    * Selects the best candidates according to Commons Lang's transformation costs.
    *
    * Exact erased descriptor matches are returned without scoring. Otherwise, the method discards inapplicable
    * candidates, scores every remaining candidate, and returns every candidate tied for the minimum score.
    *
    *   - `Ok(Nil)` means that no candidate is applicable.
    *   - `Ok(methods)` contains the exact matches or every tied lowest-cost candidate.
    *   - `Err(error)` means that metadata required for applicability or scoring could not be read.
    */
  private def best(candidates: List[JavaMethod], arguments: List[JavaArgument])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = exactMatches(candidates, arguments) match {
    case exact if exact.nonEmpty => Ok(exact)
    case _ =>
      Result.traverse(candidates)(method => isMatching(method, arguments).map(matches => method -> matches)).flatMap { tested =>
        val matching = tested.collect { case (method, true) => method }
        val scoredCandidates = Result.traverse(matching)(method => transformationCost(arguments, method).map(cost => method -> cost))
        scoredCandidates.map { scored =>
          scored.map(_._2).minOption match {
            case None => Nil
            case Some(minimum) => scored.collect { case (method, cost) if cost == minimum => method }
          }
        }
      }
  }

  /** Returns candidates whose erased parameters exactly equal the typed arguments. */
  private def exactMatches(candidates: List[JavaMethod], arguments: List[JavaArgument]): List[JavaMethod] =
    sequenceArguments(arguments).map(args => candidates.filter(erasedParameters(_) == args)).getOrElse(Nil)

  /** Returns `Ok(true)` for an applicable candidate, `Ok(false)` otherwise, or `Err` if metadata cannot be read. */
  private def isMatching(method: JavaMethod, arguments: List[JavaArgument])(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    val parameters = erasedParameters(method)
    if (arguments.length == parameters.length) {
      allAssignable(arguments, parameters).flatMap { fixedArityMatch =>
        if (fixedArityMatch || !method.isVarArgs) Ok(fixedArityMatch)
        else matchesExpandedVarArgs(arguments, parameters)
      }
    } else if (method.isVarArgs) {
      matchesExpandedVarArgs(arguments, parameters)
    } else {
      Ok(false)
    }
  }

  /** Returns `Ok(true)` when `arguments` match expanded varargs, `Ok(false)` otherwise, or `Err` on lookup failure. */
  private def matchesExpandedVarArgs(arguments: List[JavaArgument], parameters: List[ClassDesc])(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    if (parameters.isEmpty || arguments.length < parameters.length - 1) {
      // A varargs call cannot omit a fixed parameter, and a valid varargs method must have an array parameter.
      Ok(false)
    } else {
      // The fixed arguments must match their parameters before the trailing arguments can match the component type.
      val fixedCount = parameters.length - 1
      val fixedArguments = arguments.take(fixedCount)
      val fixedParameters = parameters.take(fixedCount)
      componentType(parameters.last) match {
        case None =>
          // The final parameter of a valid varargs method must be an array.
          Ok(false)
        case Some(component) =>
          // Every trailing argument must be assignable to the varargs array component type.
          allAssignable(fixedArguments, fixedParameters).flatMap { fixedMatch =>
            if (!fixedMatch) {
              // A mismatch among the fixed parameters makes the complete invocation inapplicable.
              Ok(false)
            } else {
              // The remaining arguments form the expanded varargs sequence.
              allAssignable(arguments.drop(fixedCount), List.fill(arguments.length - fixedCount)(component))
            }
          }
      }
    }
  }

  /** Returns `Ok(true)` when every argument is assignable, `Ok(false)` otherwise, or `Err` on lookup failure. */
  private def allAssignable(arguments: List[JavaArgument], parameters: List[ClassDesc])(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    if (arguments.length != parameters.length) {
      Ok(false)
    } else {
      Result.traverse(arguments.zip(parameters)) {
        case (argument, parameter) => isAssignable(argument, parameter)
      }.map(_.forall(identity))
    }
  }

  /**
    * Tests whether `argument` is assignment-compatible with `parameter` under the Commons Lang matching policy.
    *
    * The policy includes widening references, widening primitives, boxing, unboxing, and assignment of `Null` to
    * reference types. Unsupported unboxing is deliberately rejected only after best-candidate selection.
    *
    *   - `Typed(String)` is assignable to `Object` by widening the reference.
    *   - `Typed(byte)` is assignable to `int` by widening the primitive.
    *   - `Typed(int)` is assignable to `Object` by boxing to `Integer` and widening the reference.
    *   - `Null` is assignable to `String`, but not to `int`.
    *
    * Returns `Ok(true)` for an allowed conversion, `Ok(false)` otherwise, or `Err` if hierarchy metadata is missing.
    */
  private def isAssignable(argument: JavaArgument, parameter: ClassDesc)(implicit flix: Flix): Result[Boolean, JavaLookupError] = argument match {
    case Null => Ok(!parameter.isPrimitive)
    case Typed(source) if source == parameter => Ok(true)
    case Typed(source) if source.isPrimitive =>
      if (parameter.isPrimitive) Ok(isWideningPrimitive(source, parameter))
      else boxed(source) match {
        case None => Ok(false)
        case Some(boxedSource) => isReferenceSubtype(boxedSource, parameter)
      }
    case Typed(source) if parameter.isPrimitive =>
      unboxed(source) match {
        case None => Ok(false)
        case Some(unboxedSource) => Ok(unboxedSource == parameter || isWideningPrimitive(unboxedSource, parameter))
      }
    case Typed(source) => isReferenceSubtype(source, parameter)
  }

  /**
    * Tests reference subtyping, including Java's special array subtype rules.
    *
    * Nominal reference types are delegated to the configured `JavaTypeProvider`; array relationships are computed
    * directly because array descriptors do not have class-file metadata of their own.
    *
    *   - `String` is a subtype of `CharSequence`.
    *   - `String[]` is a subtype of `Object`, `Cloneable`, `Serializable`, and `Object[]`.
    *   - `int[]` is not a subtype of `long[]` because primitive array components must be identical.
    *
    * Returns `Ok(true)` for a subtype, `Ok(false)` otherwise, or `Err` if nominal hierarchy metadata is missing.
    */
  def isReferenceSubtype(source: ClassDesc, target: ClassDesc)(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    if (source == target) {
      // Every reference type is a subtype of itself.
      Ok(true)
    } else if (source.isArray) {
      // Arrays have descriptor-defined supertypes and covariant reference components.
      if (target == CD_Object || target == CloneableDesc || target == SerializableDesc) {
        Ok(true)
      } else if (target.isArray) {
        (componentType(source), componentType(target)) match {
          case (Some(sourceComponent), Some(targetComponent)) if sourceComponent.isPrimitive || targetComponent.isPrimitive =>
            Ok(sourceComponent == targetComponent)
          case (Some(sourceComponent), Some(targetComponent)) => isReferenceSubtype(sourceComponent, targetComponent)
          case _ => Ok(false)
        }
      } else {
        Ok(false)
      }
    } else if (target.isArray) {
      // A non-array reference type is never a subtype of an array type.
      Ok(false)
    } else {
      // All remaining cases are nominal reference relationships read from class-file metadata.
      flix.javaTypeProvider.isSubtype(source, target)
    }
  }

  /**
    * Computes the total Commons Lang transformation cost for an applicable method.
    *
    * Fixed parameters contribute their individual object transformation costs. Varargs add a small penalty and are
    * scored according to whether the invocation supplies no values, an explicit array, or expanded trailing values.
    *
    *   - `(String) -> (String)` costs `0.0`.
    *   - `(byte) -> (int)` costs `0.3` under the Commons Lang primitive ordering.
    *   - `(String, String) -> (String...)` costs `0.002` for two expanded varargs values.
    *
    * Returns `Ok(cost)` when all required hierarchy metadata is available, or `Err` when it cannot be read.
    */
  private def transformationCost(arguments: List[JavaArgument], method: JavaMethod)(implicit flix: Flix): Result[Float, JavaLookupError] = {
    val parameters = erasedParameters(method)
    val fixedCount = if (method.isVarArgs) parameters.length - 1 else parameters.length
    Result.traverse(arguments.take(fixedCount).zip(parameters.take(fixedCount))) {
      case (argument, parameter) => objectTransformationCost(argument, parameter)
    }.flatMap { fixedCosts =>
      val fixedCost = fixedCosts.sum
      if (!method.isVarArgs) {
        Ok(fixedCost)
      } else {
        componentType(parameters.last) match {
          case None => Ok(fixedCost)
          case Some(component) if arguments.length < parameters.length =>
            objectTransformationCost(Typed(component), CD_Object).map(fixedCost + _ + VarArgsCost)
          case Some(component) if arguments.length == parameters.length =>
            arguments.lastOption match {
              case Some(Typed(source)) if source.isArray =>
                componentType(source) match {
                  case None => Ok(fixedCost)
                  case Some(sourceComponent) => objectTransformationCost(Typed(sourceComponent), component).map(fixedCost + _ + VarArgsCost)
                }
              case _ =>
                Result.traverse(arguments.drop(fixedCount)) { argument =>
                  objectTransformationCost(argument, component).map(_ + VarArgsCost)
                }.map(costs => fixedCost + costs.sum)
            }
          case Some(component) =>
            Result.traverse(arguments.drop(fixedCount)) { argument =>
              objectTransformationCost(argument, component).map(_ + VarArgsCost)
            }.map(costs => fixedCost + costs.sum)
        }
      }
    }
  }

  /**
    * Computes the transformation cost from one argument to one target parameter.
    *
    * Primitive targets use primitive promotion costs, while reference targets use class-hierarchy costs. `Null` has
    * the fixed reference cost used by Commons Lang.
    *
    *   - `Typed(byte) -> int` delegates to primitive promotion and costs `0.3`.
    *   - `Typed(Integer) -> Number` delegates to hierarchy traversal and costs `1.0`.
    *   - `Null -> String` costs `1.5`.
    *
    * Returns `Ok(cost)` when hierarchy metadata is available, or `Err` when it cannot be read.
    */
  private def objectTransformationCost(argument: JavaArgument, target: ClassDesc)(implicit flix: Flix): Result[Float, JavaLookupError] = {
    if (target.isPrimitive) {
      Ok(primitivePromotionCost(argument, target))
    } else argument match {
      case Null => Ok(1.5f)
      case Typed(source) => objectHierarchyCost(source, target, 0.0f)
    }
  }

  /**
    * Computes the Commons Lang hierarchy distance from `source` to reference type `target`.
    *
    * Each superclass step adds `1.0`; an assignable interface adds `0.25`; and exhausting the hierarchy adds a final
    * `1.5` penalty. The accumulator `cost` records the superclass steps already traversed.
    *
    *   - `String -> Object` costs `1.0`.
    *   - `Integer -> Object` costs `2.0` through `Number`.
    *   - `ArrayList -> List` costs `0.25` because `List` is an assignable interface.
    *
    * Returns `Ok(cost)` when hierarchy metadata is available, or `Err` when it cannot be read.
    */
  private def objectHierarchyCost(source: ClassDesc, target: ClassDesc, cost: Float)(implicit flix: Flix): Result[Float, JavaLookupError] = {
    if (source == target) {
      Ok(cost)
    } else {
      isInterfaceType(target).flatMap { targetIsInterface =>
        if (targetIsInterface) {
          isAssignable(Typed(source), target).flatMap { assignable =>
            if (assignable) Ok(cost + 0.25f) else superclass(source).flatMap {
              case None => Ok(cost + 2.5f)
              case Some(parent) => objectHierarchyCost(parent, target, cost + 1.0f)
            }
          }
        } else {
          superclass(source).flatMap {
            case None => Ok(cost + 2.5f)
            case Some(parent) => objectHierarchyCost(parent, target, cost + 1.0f)
          }
        }
      }
    }
  }

  /** Returns `Ok` with the erased superclass, or `Err` if class metadata cannot be read. */
  private def superclass(desc: ClassDesc)(implicit flix: Flix): Result[Option[ClassDesc], JavaLookupError] = {
    if (desc.isPrimitive) Ok(None)
    else if (desc.isArray) Ok(Some(CD_Object))
    else flix.javaTypeProvider.lookupClass(desc).map(_.superClass.map(_.erasure))
  }

  /** Returns `Ok` with whether `desc` denotes an interface, or `Err` if class metadata cannot be read. */
  private def isInterfaceType(desc: ClassDesc)(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    if (desc.isPrimitive || desc.isArray) Ok(false)
    else flix.javaTypeProvider.lookupClass(desc).map(clazz => Modifier.isInterface(clazz.modifiers))
  }

  /**
    * Computes the Commons Lang primitive unboxing and widening cost.
    *
    * Unboxing a wrapper adds `0.1`; every subsequent position traversed in `WideningPrimitives` adds another `0.1`.
    * Applicability has already rejected primitive conversions that Java does not allow.
    *
    *   - `byte -> short` costs `0.1`.
    *   - `byte -> int` costs `0.3` because the Commons Lang ordering includes `char` between `short` and `int`.
    *   - `Integer -> long` costs `0.2`: `0.1` for unboxing and `0.1` for widening.
    *
    * Returns the cost used to rank an already-applicable primitive conversion.
    */
  private def primitivePromotionCost(argument: JavaArgument, target: ClassDesc): Float = argument match {
    case Null => 1.5f
    case Typed(source0) =>
      val (source, unboxingCost) =
        if (source0.isPrimitive) (source0, 0.0f)
        else (unboxed(source0).getOrElse(source0), 0.1f)
      val sourceIndex = WideningPrimitives.indexOf(source)
      val targetIndex = WideningPrimitives.indexOf(target)
      if (sourceIndex < 0 || targetIndex < sourceIndex) unboxingCost
      else unboxingCost + (targetIndex - sourceIndex) * 0.1f
  }

  /** Returns whether the selected candidate requires unsupported wrapper-to-primitive unboxing. */
  private def usesUnsupportedUnboxing(arguments: List[JavaArgument], method: JavaMethod): Boolean =
    arguments.zip(erasedParameters(method)).exists {
      case (Typed(argument), parameter) => parameter.isPrimitive && argument != parameter && !argument.isPrimitive
      case _ => false
    }

  /** Returns `Ok` with the first public field selected from `owner`, or `Err` if class metadata cannot be read. */
  private def findField(owner: ClassDesc, name: String, visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(None)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        clazz.declaredFields.find(f => f.ref.name == name && Modifier.isPublic(f.modifiers)) match {
          case Some(field) => Ok(Some(field))
          case None =>
            findFieldIn(clazz.interfaces.map(_.erasure), name, visited + owner).flatMap {
              case Some(field) => Ok(Some(field))
              case None => clazz.superClass match {
                case None => Ok(None)
                case Some(parent) => findField(parent.erasure, name, visited + owner)
              }
            }
        }
      }
    }
  }

  /** Returns `Ok` with the first public field selected from `owners`, or `Err` if class metadata cannot be read. */
  private def findFieldIn(owners: List[ClassDesc], name: String, visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] = owners match {
    case Nil => Ok(None)
    case owner :: rest => findField(owner, name, visited).flatMap {
      case Some(field) => Ok(Some(field))
      case None => findFieldIn(rest, name, visited)
    }
  }

  /** Returns the erased parameter descriptors that participate in overload resolution. */
  private def erasedParameters(method: JavaMethod): List[ClassDesc] = method.ref.descriptor.parameterList().asScala.toList

  /** Returns the erased signature used by Java method overriding and hiding. */
  private def methodKey(method: JavaMethod): (String, List[ClassDesc]) = method.ref.name -> erasedParameters(method)

  /** Returns whether the two methods have the same erased name and parameter types. */
  private def sameErasedSignature(method1: JavaMethod, method2: JavaMethod): Boolean = methodKey(method1) == methodKey(method2)

  /** Returns whether `method` was generated as a bridge method. */
  private def isBridge(method: JavaMethod): Boolean = (method.modifiers & BridgeModifier) != 0

  /** Returns whether the member with the given `modifiers` was generated by the compiler as a synthetic member. */
  private def isSynthetic(modifiers: Int): Boolean = (modifiers & SyntheticModifier) != 0

  /** Returns whether an anonymous subclass in another package may override the instance method `method`. */
  private def isOverridable(method: JavaMethod): Boolean = {
    val modifiers = method.modifiers
    (Modifier.isPublic(modifiers) || Modifier.isProtected(modifiers)) &&
      !Modifier.isStatic(modifiers) && !Modifier.isFinal(modifiers) && !isBridge(method) && !isSynthetic(modifiers)
  }

  /** Returns whether `method` is a public instance method that is not compiler-generated. */
  private def isPublicInstance(method: JavaMethod): Boolean = {
    val modifiers = method.modifiers
    Modifier.isPublic(modifiers) && !Modifier.isStatic(modifiers) && !isBridge(method) && !isSynthetic(modifiers)
  }

  /** Returns all typed argument descriptors, or `None` if any argument is `Null`. */
  private def sequenceArguments(arguments: List[JavaArgument]): Option[List[ClassDesc]] = {
    val descriptors = arguments.collect { case Typed(desc) => desc }
    if (descriptors.length == arguments.length) Some(descriptors) else None
  }

  /** Returns the component descriptor of an array, or `None` for a non-array descriptor. */
  private def componentType(desc: ClassDesc): Option[ClassDesc] = Option(desc.componentType())

  /** Returns whether `source` can be widened to primitive `target`. */
  private def isWideningPrimitive(source: ClassDesc, target: ClassDesc): Boolean = source match {
    case CD_byte => Set(CD_short, CD_int, CD_long, CD_float, CD_double).contains(target)
    case CD_short | CD_char => Set(CD_int, CD_long, CD_float, CD_double).contains(target)
    case CD_int => Set(CD_long, CD_float, CD_double).contains(target)
    case CD_long => Set(CD_float, CD_double).contains(target)
    case CD_float => target == CD_double
    case _ => false
  }

  /** Returns the wrapper descriptor for primitive `desc`, if one exists. */
  private def boxed(desc: ClassDesc): Option[ClassDesc] = PrimitiveToWrapper.get(desc)

  /** Returns the primitive descriptor for wrapper `desc`, if one exists. */
  private def unboxed(desc: ClassDesc): Option[ClassDesc] = WrapperToPrimitive.get(desc)

}
