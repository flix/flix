/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.interpreter

import ca.uwaterloo.flix.api
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.InternalRuntimeException
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.locks

sealed trait Value

object Value {

  /**
    * The `Unit` value.
    */
  object Unit extends Value

  /**
    * The `True` value.
    */
  object True extends Value

  /**
    * The `False` value.
    */
  object False extends Value

  /**
    * A character value.
    */
  case class Char(lit: scala.Char) extends Value

  /**
    * A Float32 value.
    */
  case class Float32(lit: scala.Float) extends Value

  /**
    * A Float64 value.
    */
  case class Float64(lit: scala.Double) extends Value

  /**
    * An Int8 value.
    */
  case class Int8(lit: scala.Byte) extends Value

  /**
    * An Int16 value.
    */
  case class Int16(lit: scala.Short) extends Value

  /**
    * An Int32 value.
    */
  case class Int32(lit: scala.Int) extends Value

  /**
    * An Int64 value.
    */
  case class Int64(lit: scala.Long) extends Value

  /**
    * A BigInt value.
    */
  case class BigInt(lit: java.math.BigInteger) extends Value

  /**
    * A String value.
    */
  case class Str(lit: java.lang.String) extends Value

  /**
    * An Array value.
    */
  case class Arr(elms: Array[AnyRef], tpe: Type) {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Arr does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Arr does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Arr does not support `toString`.")
  }

  /**
    * A Boxed value.
    */
  class Box extends Value {
    /**
      * The internal value of the box.
      */
    private var value: AnyRef = _

    /**
      * Returns the value inside the box.
      */
    def getValue: AnyRef = value

    /**
      * Mutates the value inside the box.
      */
    def setValue(x: AnyRef): Unit = {
      value = x
    }

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Box does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Box does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Box does not support `toString`.")
  }

  /**
    * A Closure value.
    */
  case class Closure(sym: Symbol.DefnSym, bindings: Array[AnyRef]) extends Value {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Closure does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Closure does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Closure does not support `toString`.")
  }

  /**
    * Flix internal representation of tags.
    */
  case class Tag(enum: Symbol.EnumSym, tag: String, value: AnyRef) extends Value with api.Tag {
    def getTag: String = tag

    def getBoxedTagValue: AnyRef = value

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tag does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tag does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tag does not support `toString`.")
  }

  /**
    * A Tuple value.
    */
  case class Tuple(elms: List[AnyRef]) extends Value with api.Tuple {
    def getBoxedValue: Array[AnyRef] = elms.toArray

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tuple does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tuple does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tuple does not support `toString`.")
  }

  case class Channel(len: Int, tpe: Type) extends  Value {
    private val contentType: Type = tpe

    private val capacity: Int = len

    private val content: AnyRef = new ConcurrentLinkedQueue[AnyRef]()

    private val waitingPutters: AnyRef = new ConcurrentLinkedQueue[Thread]()

    private val waitingGetters: AnyRef = new ConcurrentLinkedQueue[Thread]()

    def put(value: AnyRef): Channel = {
      this.synchronized {
        val c = content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wg = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wp = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]

        println(s"Thread: ${Thread.currentThread().getId()}  -  Put")

        if(c.offer(value)) {
          println(s"Thread: ${Thread.currentThread().getId()}  -  Add to content")
          printState;
          wg.peek() match {           //Lookup if any waiting getters exists
            case null =>              //If no waiting getters exists DO NOTHING
            case _ =>                 //If some waiting getters exist NOTIFY IT
              val wgToNotify = wg.peek().asInstanceOf[Thread]
              println(s"Thread: ${Thread.currentThread().getId()}  -  Notifies Thread ${wgToNotify.getId()}")
              notifyAll()
          }
        }
        else {                      //If channel is full
          if (wg.poll() != null) { //If there are any excess waiting getters
            println(s"Thread: ${Thread.currentThread().getId()}  -  Add to content and notify wg")
            c.add(value)
            printState;
            notifyAll()
          }
          else {
            println(s"Thread: ${Thread.currentThread().getId()}  -  Add to wp")
            wp.add(Thread.currentThread())
            println(s"Thread: ${Thread.currentThread().getId()}  -  goes to sleep!")
            printState;
            wait()
            println(s"PUTTER ${Thread.currentThread().getId()} WOKEN")
            wp.remove(Thread.currentThread())
            put(value)
          }
        }
        this
      }
    }

    def get(): AnyRef = {
      this.synchronized {
        val c = content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wg = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wp = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]

        println(s"Thread: ${Thread.currentThread().getId()}  -  Get")

        c.peek() match { //Lookup elements in content
          case null => //If no element exists ADD TO WAITING GETTERS AND PUT TO SLEEP
            println(s"Thread: ${Thread.currentThread().getId()}  -  Add to Waiting Getters and wait")
            wg.add(Thread.currentThread())
            printState;

            wp.peek() match { //Lookup waiting putters
              case null => //If no waiting putters exists NO NOTHING
              case _ => //If some waiting putters exists NOTIFY IT
                val wpToNotify = wp.peek().asInstanceOf[Thread]
                println(s"Thread: ${Thread.currentThread().getId()}  -  Notifies Thread ${wpToNotify.getId()}")
                notifyAll()
            }

            println(s"Thread: ${Thread.currentThread().getId()} goes to sleep!")
            wait()
            println(s"GETTER ${Thread.currentThread().getId()} WOKEN")
            //wg.remove(Thread.currentThread())
            get()
          case _ => //If some element exists
            println(s"Thread: ${Thread.currentThread().getId}  -  Take and return content")
            val tmp = c.poll()
            notifyAll()
            printState;
            println(s"element got i channel ${content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]].hashCode() % 100}: ${tmp}")
            return tmp
        }
      }
    }

    def printState =
      println(s"      STATE OF CHANNEL ${content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]].hashCode() % 100}: " +
        s"Thread: ${Thread.currentThread().getId} " +
        s"c.size = ${content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]].size()}; " +
        s"wp.size = ${waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]].size()}; " +
        s"wg.size = ${waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]].size()}")

    def notifyGet(): Unit = {
      this.synchronized{
        val g = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[Thread]].peek()
        println(s"Notifies waiting getter ${g}")
        g.notifyAll()
      }
    }

    def notifyPut(): Unit = {
      this.synchronized {
        val p = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[Thread]].peek()
        println(s"Notifies waiting putter ${p}")
        p.notifyAll()
      }
    }

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Channel does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Channel does not support `hashCode`.")

    final override def toString: String = s"Channel[$tpe] $capacity"
  }
}
