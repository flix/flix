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
 * Constructor overload resolution intentionally mirrors Apache Commons Lang's
 * `ConstructorUtils.getMatchingAccessibleConstructor` and the transformation
 * costs in `MemberUtils`. It is a compatibility algorithm for Flix's existing
 * reflective lookup, not an implementation of JLS most-specific resolution.
 *
 * Resolution separates legality from preference. A candidate must first be
 * applicable; only applicable candidates receive a cost. Consequently, a low
 * cost can never make an illegal argument conversion acceptable.
 *
 * Exact erased descriptor matches are selected before any scoring. This covers
 * ordinary exact parameters and passing an array directly to a varargs
 * constructor. A `Null` argument has no descriptor and cannot be exact.
 *
 * Applicability supports the conversions recognized by the current reflective
 * path: widening references, widening primitives, boxing, unboxing, `null` to
 * references, Java array subtyping, and both fixed and expanded varargs. This
 * step only answers whether a constructor can participate in ranking.
 *
 * Each applicable constructor then receives one cost per argument. The costs
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
 * Commons Lang keeps the first minimum encountered in reflection order, but
 * reflection order is unspecified and may differ from class-file metadata
 * order. Returning every tied minimum lets shadow comparison accept any
 * semantically equivalent Commons Lang choice without depending on that order.
 *
 * Wrapper-to-primitive unboxing is considered during applicability and scoring
 * because Commons Lang considers it. Flix does not support that conversion in
 * lowering, so tied best candidates requiring it are removed only after best
 * candidate selection. Filtering earlier could select a worse supported
 * constructor, which would not match the existing reflective path.
 *
 * The score should therefore be read as an estimate of conversion distance
 * used to preserve existing behavior. It is not a runtime conversion, a proof
 * of type safety, or a general model of Java overload resolution.
 */
/** Resolves accessible Java members using descriptor-based class-file metadata. */
object JavaMemberResolver {

  import JavaArgument.*

  private val VarArgsCost = 0.001f

  private val CloneableDesc = ClassDesc.of("java.lang.Cloneable")

  private val SerializableDesc = ClassDesc.of("java.io.Serializable")

  private val WideningPrimitives = List(CD_byte, CD_short, CD_char, CD_int, CD_long, CD_float, CD_double)

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

  private val WrapperToPrimitive = PrimitiveToWrapper.map {
    case (primitive, wrapper) => wrapper -> primitive
  }

  /** Returns `Ok` with every tied best public constructor, or `Err` if class metadata cannot be read. */
  def constructors(owner: ClassDesc, arguments: List[JavaArgument])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] =
    flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
      val candidates = clazz.declaredConstructors.filter(method => Modifier.isPublic(method.modifiers))
      best(candidates, arguments).map(_.filterNot(usesUnsupportedUnboxing(arguments, _)))
    }

  /** Returns `Ok` with the selected public field, or `Err` if class metadata cannot be read. */
  def field(owner: ClassDesc, name: String, static: Boolean)(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] =
    findField(owner, name, Set.empty).map(_.filter(f => Modifier.isStatic(f.modifiers) == static))

  /** Returns `Ok` with every tied lowest-cost candidate, or `Err` if assignability metadata cannot be read. */
  private def best(candidates: List[JavaMethod], arguments: List[JavaArgument])(implicit flix: Flix): Result[List[JavaMethod], JavaLookupError] = exactMatches(candidates, arguments) match {
    case exact if exact.nonEmpty => Ok(exact)
    case _ =>
      Result.traverse(candidates)(method => isMatching(method, arguments).map(matches => method -> matches)).flatMap { tested =>
        val matching = tested.collect { case (method, true) => method }
        Result.traverse(matching)(method => transformationCost(arguments, method).map(cost => method -> cost)).map { scored =>
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
      Ok(false)
    } else {
      val fixedCount = parameters.length - 1
      val fixedArguments = arguments.take(fixedCount)
      val fixedParameters = parameters.take(fixedCount)
      componentType(parameters.last) match {
        case None => Ok(false)
        case Some(component) =>
          allAssignable(fixedArguments, fixedParameters).flatMap { fixedMatch =>
            if (!fixedMatch) Ok(false)
            else allAssignable(arguments.drop(fixedCount), List.fill(arguments.length - fixedCount)(component))
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

  /** Returns `Ok` with whether `argument` is assignable to `parameter`, or `Err` if hierarchy metadata cannot be read. */
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

  /** Returns `Ok` with whether `source` is a reference subtype of `target`, or `Err` if hierarchy metadata cannot be read. */
  private def isReferenceSubtype(source: ClassDesc, target: ClassDesc)(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    if (source == target) {
      Ok(true)
    } else if (source.isArray) {
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
      Ok(false)
    } else {
      flix.javaTypeProvider.isSubtype(source, target)
    }
  }

  /** Returns `Ok` with the Commons-Lang-style transformation cost, or `Err` if hierarchy metadata cannot be read. */
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

  /** Returns `Ok` with the transformation cost to `target`, or `Err` if hierarchy metadata cannot be read. */
  private def objectTransformationCost(argument: JavaArgument, target: ClassDesc)(implicit flix: Flix): Result[Float, JavaLookupError] = {
    if (target.isPrimitive) {
      Ok(primitivePromotionCost(argument, target))
    } else argument match {
      case Null => Ok(1.5f)
      case Typed(source) => objectHierarchyCost(source, target, 0.0f)
    }
  }

  /** Returns `Ok` with the reference-hierarchy cost, or `Err` if hierarchy metadata cannot be read. */
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

  /** Returns the Commons-Lang-style primitive promotion cost from `argument` to `target`. */
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
