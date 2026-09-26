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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, StaticConstructorMethod, StaticField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.mkDesc
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The class of a nullary enum case, e.g. `A/B/Case$Color$Red` for `case Red` of
  * `enum A.B.Color`.
  *
  * A nullary case carries no values, so the class has a single instance held in
  * [[SingletonField]].
  */
object GenNullaryTag {

  /**
    * Returns the descriptor of the class of the case `sym`.
    *
    * The namespace of the enum is the package of the class, as it is for the def, closure,
    * and effect classes. Note that this is [[Symbol.EnumSym.namespace]] and not
    * [[Symbol.CaseSym.namespace]]: the latter appends the name of the enum, which would
    * give every enum a package of its own.
    *
    * The `Case` prefix is reserved: it keeps the enum and case names, which are chosen by
    * the user, from colliding with the generated classes that share this package, e.g.
    * `Def$map` and `Eff$Console`, or, for an enum in the root namespace, `Tag$Obj` and
    * `Struct$Obj`.
    */
  def desc(sym: Symbol.CaseSym): ClassDesc =
    mkDesc(sym.enumSym.namespace, Mangle.mkClassName("Case", List(sym.enumSym.name, sym.name)))

  def genByteCode(sym: Symbol.CaseSym)(implicit flix: Flix): Array[Byte] = {
    val d = desc(sym)
    val cm = ClassMaker.mkClass(d, IsFinal, superClass = GenTagged.Desc)

    cm.mkStaticConstructor(StaticConstructorMethod(d), singletonStaticConstructor(Constructor(sym), SingletonField(sym))(_))
    cm.mkField(SingletonField(sym), IsPublic, IsFinal, NotVolatile)
    cm.mkConstructor(Constructor(sym), IsPublic, constructorIns(sym.ordinal)(_))

    cm.closeClassMaker()
  }

  def SingletonField(sym: Symbol.CaseSym): StaticField = {
    val d = desc(sym)
    StaticField(d, "singleton", d)
  }

  private def Constructor(sym: Symbol.CaseSym): ConstructorMethod =
    ConstructorMethod(desc(sym), Nil)

  /** `[] --> return` */
  private def constructorIns(ordinal: Int)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(GenTagged.Constructor)
    thisLoad()
    pushInt(ordinal)
    PUTFIELD(GenTagged.OrdinalField)
    RETURN()
  }

}
