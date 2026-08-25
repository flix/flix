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

import ca.uwaterloo.flix.api.Flix

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_boolean, CD_byte, CD_char, CD_double, CD_float, CD_int, CD_long, CD_short}

/**
  * Name mangling and construction of class names for generated classes.
  */
object Mangle {

  /** The root (unnamed) package. */
  val RootPackage: List[String] = Nil

  /** The `dev.flix.runtime` package of the Flix runtime classes. */
  val DevFlixRuntime: List[String] = List("dev", "flix", "runtime")

  /** Returns the [[ClassDesc]] of the class `name` in the package `pkg`. */
  def mkDesc(pkg: List[String], name: String): ClassDesc = {
    val prefix = if (pkg.isEmpty) "" else pkg.mkString("", "/", "/")
    ClassDesc.ofInternalName(prefix + name)
  }

  /**
    * Returns the name of the erased type `desc`, as used in parametrized class names.
    *
    * Every reference type erases to `"Obj"`, so `Tuple2$Obj$Int32$Obj` names the tuple
    * class of any three-element tuple whose middle element is an `Int32`.
    */
  def erasedName(desc: ClassDesc): String = desc match {
    case CD_boolean => "Bool"
    case CD_char => "Char"
    case CD_byte => "Int8"
    case CD_short => "Int16"
    case CD_int => "Int32"
    case CD_long => "Int64"
    case CD_float => "Float32"
    case CD_double => "Float64"
    case _ => "Obj"
  }

  /**
    * Constructs a concatenated string using `Flix.Delimiter`. The call
    * `mkClassName("Tuple2", List(Object, Int, String))` would
    * result in the string `"Tuple2$Obj$Int32$Obj"`.
    */
  def mkClassName(prefix: String, args: List[String]): String = {
    val cPrefix = mangle(prefix)
    if (args.isEmpty) s"$cPrefix${Flix.Delimiter}"
    else s"$cPrefix${Flix.Delimiter}${args.map(mangle).mkString(Flix.Delimiter)}"
  }

  /** Constructs a class name from `prefix` and the single argument `arg`. */
  def mkClassName(prefix: String, arg: String): String =
    mkClassName(prefix, List(arg))

  /** Constructs a class name from `prefix` alone. */
  def mkClassName(prefix: String): String =
    mkClassName(prefix, Nil)

  /**
    * Performs name mangling on the given string `s` to avoid issues with special characters.
    */
  def mangle(s: String): String = {
    // Fast path: most names contain no special characters, so we avoid
    // allocating a new string entirely.
    if (!containsSpecialChar(s)) s else mangleSlow(s)
  }

  /**
    * Returns `true` if `s` contains at least one character that must be mangled.
    */
  private def containsSpecialChar(s: String): Boolean = {
    var i = 0
    while (i < s.length) {
      if (mangleReplacement(s.charAt(i)) != null) {
        return true
      }
      i = i + 1
    }
    false
  }

  /**
    * Mangles `s` in a single pass, replacing every special character with its mangled form.
    */
  private def mangleSlow(s: String): String = {
    val sb = new StringBuilder(s.length + 16)
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      val replacement = mangleReplacement(c)
      if (replacement != null) {
        sb.append(Flix.Delimiter)
        sb.append(replacement)
      } else {
        sb.append(c)
      }
      i = i + 1
    }
    sb.toString
  }

  /**
    * Returns the mangled replacement for the special character `c`, or `null` if `c` is not special.
    */
  private def mangleReplacement(c: Char): String = c match {
    case '+' => "plus"
    case '-' => "minus"
    case '*' => "asterisk"
    case '/' => "fslash"
    case '\\' => "bslash"
    case '<' => "less"
    case '>' => "greater"
    case '=' => "eq"
    case '&' => "ampersand"
    case '|' => "bar"
    case '^' => "caret"
    case '~' => "tilde"
    case '!' => "exclamation"
    case '#' => "hashtag"
    case ':' => "colon"
    case '?' => "question"
    case '@' => "at"
    case '.' => "dot"
    case _ => null
  }
}
