/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{AbstractMethod, ConstructorMethod, InstanceField, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The abstract class `Record`, extended by [[GenRecordEmpty]] and [[GenRecordExtend]].
  *
  * A record is a chain of `RecordExtend$..` nodes, one per label, that ends in `RecordEmpty`.
  * The class holds what the nodes have in common, so that a label can be looked up by walking
  * the chain without knowing the class of each node:
  *
  * {{{
  * public abstract class Record {
  *   public String label; // null in RecordEmpty
  *   public Record rest;  // null in RecordEmpty
  *   public Record() { ... }
  *   public static final Record lookupField(Record record, String label) { ... }
  *   public abstract Record restrictField(String label);
  * }
  * }}}
  */
object GenRecord {

  /** The JVM class descriptor for the generated `Record` class. */
  val Desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("Record"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkAbstractClass(this.Desc)

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(LabelField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(RestField, IsPublic, NotFinal, NotVolatile)
    cm.mkStaticMethod(LookupFieldMethod, IsPublic, IsFinal, lookupFieldIns(_))
    cm.mkAbstractMethod(RestrictFieldMethod)

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def LabelField: InstanceField = InstanceField(this.Desc, "label", JavaClasses.String)

  def RestField: InstanceField = InstanceField(this.Desc, "rest", this.Desc)

  def LookupFieldMethod: StaticMethod = StaticMethod(this.Desc, "lookupField",
    mkDescriptor(this.Desc, JavaClasses.String)(this.Desc))

  def RestrictFieldMethod: AbstractMethod = AbstractMethod(this.Desc, "restrictField",
    mkDescriptor(JavaClasses.String)(this.Desc))

  /**
    * Returns the first node of the record that holds the label:
    *
    * {{{
    * while (record.label != label) {
    *   record = record.rest;
    *   if (record == null) throw new UnsupportedOperationException(...);
    * }
    * return record;
    * }}}
    *
    * The labels are compared by reference, not with `equals`. This is sound because every
    * label is loaded from the constant pool with an `ldc` instruction, see [[GenRecordExtend]].
    *
    * A well-typed lookup always finds its label. The loop only runs off the end of the
    * record -- `RecordEmpty` has a `null` label and a `null` rest -- if the record does not
    * have the type that the lookup assumes.
    */
  private def lookupFieldIns(implicit mv: MethodVisitor): Unit = {
    withName(0, this.Desc) { record =>
      withName(1, JavaClasses.String) { label =>
        whileLoop(Condition.ACMPNE) {
          record.load()
          GETFIELD(LabelField)
          label.load()
        } {
          record.load()
          GETFIELD(RestField)
          DUP()
          record.store()
          ifCondition(Condition.NULL) {
            throwUnsupportedOperationException(s"${LookupFieldMethod.name}: label not found")
          }
        }
        record.load()
        ARETURN()
      }
    }
  }

}
