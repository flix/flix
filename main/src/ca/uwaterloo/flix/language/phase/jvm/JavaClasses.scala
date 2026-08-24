/*
 * Copyright 2017 Magnus Madsen
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

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.*

/**
  * The [[ClassDesc]]s of the Java classes and interfaces referenced by the backend.
  *
  * Descriptors that exist in [[java.lang.constant.ConstantDescs]] are reused from there.
  */
object JavaClasses {

  val Arrays: ClassDesc = ClassDesc.ofInternalName("java/util/Arrays")
  val AtomicLong: ClassDesc = ClassDesc.ofInternalName("java/util/concurrent/atomic/AtomicLong")
  val BigDecimal: ClassDesc = ClassDesc.ofInternalName("java/math/BigDecimal")
  val BigInteger: ClassDesc = ClassDesc.ofInternalName("java/math/BigInteger")
  val CallSite: ClassDesc = CD_CallSite
  val ConcurrentLinkedQueue: ClassDesc = ClassDesc.ofInternalName("java/util/concurrent/ConcurrentLinkedQueue")
  val DoubleConsumer: ClassDesc = ClassDesc.ofInternalName("java/util/function/DoubleConsumer")
  val DoubleFunction: ClassDesc = ClassDesc.ofInternalName("java/util/function/DoubleFunction")
  val DoublePredicate: ClassDesc = ClassDesc.ofInternalName("java/util/function/DoublePredicate")
  val DoubleUnaryOperator: ClassDesc = ClassDesc.ofInternalName("java/util/function/DoubleUnaryOperator")
  val Error: ClassDesc = ClassDesc.ofInternalName("java/lang/Error")
  val IntConsumer: ClassDesc = ClassDesc.ofInternalName("java/util/function/IntConsumer")
  val IntFunction: ClassDesc = ClassDesc.ofInternalName("java/util/function/IntFunction")
  val IntPredicate: ClassDesc = ClassDesc.ofInternalName("java/util/function/IntPredicate")
  val IntUnaryOperator: ClassDesc = ClassDesc.ofInternalName("java/util/function/IntUnaryOperator")
  val Iterator: ClassDesc = ClassDesc.ofInternalName("java/util/Iterator")
  val LambdaMetafactory: ClassDesc = ClassDesc.ofInternalName("java/lang/invoke/LambdaMetafactory")
  val LinkedList: ClassDesc = ClassDesc.ofInternalName("java/util/LinkedList")
  val LongConsumer: ClassDesc = ClassDesc.ofInternalName("java/util/function/LongConsumer")
  val LongFunction: ClassDesc = ClassDesc.ofInternalName("java/util/function/LongFunction")
  val LongPredicate: ClassDesc = ClassDesc.ofInternalName("java/util/function/LongPredicate")
  val LongUnaryOperator: ClassDesc = ClassDesc.ofInternalName("java/util/function/LongUnaryOperator")
  val Math: ClassDesc = ClassDesc.ofInternalName("java/lang/Math")
  val MethodHandle: ClassDesc = CD_MethodHandle
  val MethodHandles$Lookup: ClassDesc = CD_MethodHandles_Lookup
  val MethodType: ClassDesc = CD_MethodType
  val ObjConsumer: ClassDesc = ClassDesc.ofInternalName("java/util/function/Consumer")
  val ObjFunction: ClassDesc = ClassDesc.ofInternalName("java/util/function/Function")
  val ObjPredicate: ClassDesc = ClassDesc.ofInternalName("java/util/function/Predicate")
  val Object: ClassDesc = CD_Object
  val Regex: ClassDesc = ClassDesc.ofInternalName("java/util/regex/Pattern")
  val ReentrantLock: ClassDesc = ClassDesc.ofInternalName("java/util/concurrent/locks/ReentrantLock")
  val Runnable: ClassDesc = ClassDesc.ofInternalName("java/lang/Runnable")
  val String: ClassDesc = CD_String
  val StringBuilder: ClassDesc = ClassDesc.ofInternalName("java/lang/StringBuilder")
  val System: ClassDesc = ClassDesc.ofInternalName("java/lang/System")
  val Thread: ClassDesc = ClassDesc.ofInternalName("java/lang/Thread")
  val Thread$Builder$OfVirtual: ClassDesc = ClassDesc.ofInternalName("java/lang/Thread$Builder$OfVirtual")
  val Thread$UncaughtExceptionHandler: ClassDesc = ClassDesc.ofInternalName("java/lang/Thread$UncaughtExceptionHandler")
  val Throwable: ClassDesc = CD_Throwable
  val UnsupportedOperationException: ClassDesc = ClassDesc.ofInternalName("java/lang/UnsupportedOperationException")

}
