/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.mkDesc
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, GenFunAndClosureClasses, Mangle, MethodTypeDescs}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The namespace class of a Flix module, which holds a `static void` shim method for each test
  * in the module. A namespace class is only generated for namespaces that contain tests.
  */
object GenNamespace {

  def desc(ns: List[String]): ClassDesc =
    mkDesc(ns.dropRight(1), ns.lastOption.getOrElse(s"Root${Flix.Delimiter}"))

  /** Generates the namespace class of `ns` with a shim method for each test in `tests`. */
  def genByteCode(ns: List[String], tests: List[Symbol.DefnSym])(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(ns), IsFinal)

    cm.mkConstructor(Constructor(ns), IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

    for (sym <- tests) {
      cm.mkStaticMethod(ShimMethod(sym), IsPublic, IsFinal, shimIns(sym)(_))
    }

    cm.closeClassMaker()
  }

  private def Constructor(ns: List[String]): ConstructorMethod = ConstructorMethod(desc(ns), Nil)

  /** The `static void m_<name>()` method on the namespace class of `sym` that runs the test `sym`. */
  def ShimMethod(sym: Symbol.DefnSym): StaticMethod =
    StaticMethod(desc(sym.namespace), "m_" + Mangle.mangle(sym.name), MethodTypeDescs.NothingToVoid)

  private def shimIns(sym: Symbol.DefnSym)(implicit mv: MethodVisitor): Unit = {
    GenFunAndClosureClasses.runUnitDef(sym, s"in shim method of $sym")
    RETURN()
  }

}
