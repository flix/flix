/*
 * Copyright 2025 Asher Frost
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.QualifiedSym

object SymbolSet {

  /**
    * Returns an empty symbol set.
    */
  val empty: SymbolSet = SymbolSet(
    Set.empty[Symbol.EnumSym],
    Set.empty[Symbol.StructSym],
    Set.empty[Symbol.TraitSym],
    Set.empty[Symbol.EffSym],
  )

  /**
    * Returns the symbol set that holds the given trait symbol `sym` and nothing else.
    */
  def ofTrait(sym: Symbol.TraitSym): SymbolSet = SymbolSet(Set.empty, Set.empty, Set(sym), Set.empty)

  /**
    * Returns all the symbols in a given type
    *
    * An associated type contributes its trait, since it is written with the trait, e.g.
    * `Collection.Elm[a]`.
    */
  def symbolsOf(tpe: Type): SymbolSet = tpe match {
    case Type.Var(_, _) => empty
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => SymbolSet(Set(sym), Set.empty, Set.empty, Set.empty)
    case Type.Cst(TypeConstructor.Struct(sym, _), _) => SymbolSet(Set.empty, Set(sym), Set.empty, Set.empty)
    case Type.Cst(TypeConstructor.Effect(sym, _), _) => SymbolSet(Set.empty, Set.empty, Set.empty, Set(sym))
    case Type.Cst(_, _) => empty
    case Type.Apply(tpe1, tpe2, _) => symbolsOf(tpe1) ++ symbolsOf(tpe2)
    case Type.Alias(_, _, t, _) => symbolsOf(t)
    case Type.AssocType(symUse, arg, _, _) => ofTrait(symUse.sym.trt) ++ symbolsOf(arg)
    case Type.JvmToType(_, _) => empty
    case Type.JvmToEff(_, _) => empty
    case Type.UnresolvedJvmType(_, _) => empty
  }

  /**
    * Returns all the symbols in the given trait constraint `tc`: its trait and the symbols in its
    * type.
    */
  def symbolsOf(tc: TraitConstraint): SymbolSet = ofTrait(tc.symUse.sym) ++ symbolsOf(tc.arg)

  /**
    * Returns all the symbols in the given equality constraint `ec`: the trait of its associated type
    * and the symbols in its two types.
    */
  def symbolsOf(ec: EqualityConstraint): SymbolSet =
    ofTrait(ec.symUse.sym.trt) ++ symbolsOf(ec.tpe1) ++ symbolsOf(ec.tpe2)

  /**
    * Returns all the symbols in the given scheme `sc`: those in its type and those in its
    * constraints.
    */
  def symbolsOf(sc: Scheme): SymbolSet = {
    val tconstrs = sc.tconstrs.map(tc => symbolsOf(tc))
    val econstrs = sc.econstrs.map(ec => symbolsOf(ec))
    (tconstrs ::: econstrs).foldLeft(symbolsOf(sc.base))(_ ++ _)
  }

  /**
    * Return a symbol set containing all the symbols in `s1` ambiguous w.r.t `s2`.
    *
    * @param s1 The first symbol set
    * @param s2 The second symbol set
    * @return All the symbols that are in `s1` such that a symbol in `s2` has the same base name but a different namespace
    */
  def ambiguous(s1: SymbolSet, s2: SymbolSet): SymbolSet = {
    val s3 = s1 ++ s2
    SymbolSet(
      s3.enums.filter(sym1 => s3.enums.exists(sym2 => isAmbiguous(sym1, sym2))),
      s3.structs.filter(sym1 => s3.structs.exists(sym2 => isAmbiguous(sym1, sym2))),
      s3.traits.filter(sym1 => s3.traits.exists(sym2 => isAmbiguous(sym1, sym2))),
      s3.effects.filter(sym1 => s3.effects.exists(sym2 => isAmbiguous(sym1, sym2))),
    )
  }

  /**
    * Checks if two symbols are ambiguous with respect to each other.
    * Two symbols are ambiguous if they have the same name but different namespaces.
    *
    * @param sym1 The first symbol
    * @param sym2 The second symbol
    * @return true if the symbols are ambiguous, false otherwise
    */
  private def isAmbiguous(sym1: QualifiedSym, sym2: QualifiedSym): Boolean = {
    sym1.name == sym2.name && sym1.namespace != sym2.namespace
  }

  /**
    * Returns a symbol set containing all the symbols in `tpe1` ambiguous w.r.t `tpe2`.
    *
    * @param tpe1 The first type
    * @param tpe2 The second type
    * @return All the symbols that are in `tpe1` such that a symbol in `tpe2` has the same base name but a different namespace
    */
  def ambiguities(tpe1: Type, tpe2: Type): SymbolSet = SymbolSet.ambiguous(SymbolSet.symbolsOf(tpe1), SymbolSet.symbolsOf(tpe2))

  /**
    * Returns the full name of a qualified symbol.
    *
    * @param sym The qualified symbol
    * @return The full name of the symbol, including its namespace if it has one
    */
  def fullNameOf(sym: QualifiedSym): String = {
    if (sym.namespace.isEmpty) {
      sym.name
    } else sym.namespace.mkString(".") + "." + sym.name
  }
}

/**
  * A set of symbols, used for disambiguating names
  */
case class SymbolSet(
                      enums: Set[Symbol.EnumSym],
                      structs: Set[Symbol.StructSym],
                      traits: Set[Symbol.TraitSym],
                      effects: Set[Symbol.EffSym],
                    ) {

  /**
    * Returns the union of `this` and `that`
    */
  def ++(that: SymbolSet): SymbolSet = {
    SymbolSet(
      enums ++ that.enums,
      structs ++ that.structs,
      traits ++ that.traits,
      effects ++ that.effects,
    )
  }

  def allSymbols: Set[QualifiedSym] = {
    enums ++ structs ++ traits ++ effects
  }

  /**
    * Returns the symbol as a string if it is in the set, otherwise returns just the head.
    */
  def qualify(sym: QualifiedSym): String = {
    if (allSymbols.contains(sym)) {
      if (sym.namespace.isEmpty) {
        sym.name
      } else sym.namespace.mkString(".") + "." + sym.name
    } else {
      sym.name
    }
  }
}
