/*
 * Copyright 2021 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.util

import ca.uwaterloo.flix.language.ast.{KindedAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.InternalCompilerException


/**
  * The following traits are assumed to always exist.
  *
  * Anything added here must be mentioned in `CoreLibrary` in the Flix class.
  */
object PredefinedTraits {

  /**
    * Returns the trait symbol with the given `name`.
    */
  def lookupTraitSym(name: String, root: KindedAst.Root): Symbol.TraitSym = {
    val key = new Symbol.TraitSym(Nil, name, SourceLocation.Unknown)
    root.traits.getOrElse(key, throw InternalCompilerException(s"The trait: '$key' is not defined.", SourceLocation.Unknown)).sym
  }

  /**
    * Returns the sig symbol with the given `clazz` and name `sig`.
    */
  def lookupSigSym(trt: String, sig: String, root: KindedAst.Root): Symbol.SigSym = {
    val trtKey = new Symbol.TraitSym(Nil, trt, SourceLocation.Unknown)
    val sigKey = new Symbol.SigSym(trtKey, sig, SourceLocation.Unknown)
    root.traits.getOrElse(trtKey, throw InternalCompilerException(s"The trait: '$trtKey' is not defined.", SourceLocation.Unknown))
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
    enumDecl.cases.values.find(_.sym.name == cazeName).getOrElse(throw InternalCompilerException(s"The case '$cazeName' in '$enumKey' is not defined.", SourceLocation.Unknown)).sym
  }


}
