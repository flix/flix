/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.util

import ca.uwaterloo.flix.language.ast.{KindedAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.InternalCompilerException


/**
  * The following classes are assumed to always exist.
  *
  * Anything added here must be mentioned in `CoreLibrary` in the Flix class.
  */
object PredefinedClasses {

  /**
    * Returns the class symbol with the given `name`.
    */
  def lookupClassSym(name: String, root: KindedAst.Root): Symbol.ClassSym = {
    val key = new Symbol.ClassSym(Nil, name, SourceLocation.Unknown)
    root.classes.getOrElse(key, throw InternalCompilerException(s"The type class: '$key' is not defined.", SourceLocation.Unknown)).sym
  }

  /**
    * Returns the sig symbol with the given `clazz` and name `sig`.
    */
  def lookupSigSym(clazz: String, sig: String, root: KindedAst.Root): Symbol.SigSym = {
    val clazzKey = new Symbol.ClassSym(Nil, clazz, SourceLocation.Unknown)
    val sigKey = new Symbol.SigSym(clazzKey, sig, SourceLocation.Unknown)
    root.classes.getOrElse(clazzKey, throw InternalCompilerException(s"The type class: '$clazzKey' is not defined.", SourceLocation.Unknown))
      .sigs.getOrElse(sigKey, throw InternalCompilerException(s"The signature '$sigKey' is not defined.", SourceLocation.Unknown))
      .sym
  }

  /**
    * Returns the def symbol with the given `name` in the given namespace `ns`.
    */
  def lookupDefSym(ns: List[String], name: String, root: KindedAst.Root): Symbol.DefnSym = {
    val key = new Symbol.DefnSym(None, ns, name, SourceLocation.Unknown)
    root.defs.getOrElse(key, throw InternalCompilerException(s"The definition '$key' is not defined.", SourceLocation.Unknown)).sym
  }

  /**
    * Returns the enum symbol with the given name `name`.
    */
  def lookupEnumSym(name: String, root: KindedAst.Root): Symbol.EnumSym = {
    val key = new Symbol.EnumSym(None, Nil, name, SourceLocation.Unknown)
    root.enums.getOrElse(key, throw InternalCompilerException(s"The definition '$key' is not defined.", SourceLocation.Unknown)).sym
  }

  /**
    * Returns the case symbol with the given name `cazeName`.
    */
  def lookupCaseSym(enumName: String, cazeName: String, root: KindedAst.Root): Symbol.CaseSym = {
    val enumKey = new Symbol.EnumSym(None, Nil, enumName, SourceLocation.Unknown)
    val enumDecl = root.enums.getOrElse(enumKey, throw InternalCompilerException(s"The definition '$enumKey' is not defined.", SourceLocation.Unknown))
    val cazeKey = new Symbol.CaseSym(enumDecl.sym, cazeName, SourceLocation.Unknown)
    enumDecl.cases.getOrElse(cazeKey, throw InternalCompilerException(s"The definition '$enumKey' is not defined.", SourceLocation.Unknown)).sym
  }


}
