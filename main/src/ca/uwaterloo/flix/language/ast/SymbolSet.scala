package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{QualifiedSym, Symbol}

case class SymbolSet(
                    enums: Set[Symbol.EnumSym],
                    structs: Set[Symbol.StructSym],
                    restrictableEnum: Set[Symbol.RestrictableEnumSym],
                    cases: Set[Symbol.CaseSym],
                    restrictableCases: Set[Symbol.RestrictableCaseSym],
                    structField: Set[Symbol.StructFieldSym],
                    traits: Set[Symbol.TraitSym],
                    sigs: Set[Symbol.SigSym],
                    labels: Set[Symbol.LabelSym],
                    typeAliases: Set[Symbol.TypeAliasSym],
                    assocTypes: Set[Symbol.AssocTypeSym],
                    effects: Set[Symbol.EffSym],
                    ops: Set[Symbol.OpSym],
                    regionSym: Set[Symbol.RegionSym],
                    modules: Set[Symbol.ModuleSym],
                    ) {
  def split(fqn: String): Option[(List[String], String)] = {
    val split = fqn.split('.')
    if (split.length < 2)
      return None
    val namespace = split.init.toList
    val name = split.last
    Some((namespace, name))
  }
  /*
    * Adds a symbol to the set.
    * @param sym The symbol to add to the set
    * @return A new SymbolSet with the symbol added.
   */
  def addSymbol(sym: Symbol): SymbolSet = sym match {
    case s: Symbol.EnumSym => copy(enums = enums + s)
    case s: Symbol.StructSym => copy(structs = structs + s)
    case s: Symbol.RestrictableEnumSym => copy(restrictableEnum = restrictableEnum + s)
    case s: Symbol.CaseSym => copy(cases = cases + s)
    case s: Symbol.RestrictableCaseSym => copy(restrictableCases = restrictableCases + s)
    case s: Symbol.StructFieldSym => copy(structField = structField + s)
    case s: Symbol.TraitSym => copy(traits = traits + s)
    case s: Symbol.SigSym => copy(sigs = sigs + s)
    case s: Symbol.LabelSym => copy(labels = labels + s)
    case s: Symbol.TypeAliasSym => copy(typeAliases = typeAliases + s)
  }
  def addSymbols(syms : Set[Symbol]): SymbolSet = {
    syms.foldLeft(this)((acc, sym) => acc.addSymbol(sym))
  }
  def unionSets(otherSet : SymbolSet): SymbolSet = {
    SymbolSet(
      enums ++ otherSet.enums,
      structs ++ otherSet.structs,
      restrictableEnum ++ otherSet.restrictableEnum,
      cases ++ otherSet.cases,
      restrictableCases ++ otherSet.restrictableCases,
      structField ++ otherSet.structField,
      traits ++ otherSet.traits,
      sigs ++ otherSet.sigs,
      labels ++ otherSet.labels,
      typeAliases ++ otherSet.typeAliases,
      assocTypes ++ otherSet.assocTypes,
      effects ++ otherSet.effects,
      ops ++ otherSet.ops,
      regionSym ++ otherSet.regionSym,
      modules ++ otherSet.modules
    )
  }
  def emptySet: SymbolSet = SymbolSet(
    Set.empty[Symbol.EnumSym],
    Set.empty[Symbol.StructSym],
    Set.empty[Symbol.RestrictableEnumSym],
    Set.empty[Symbol.CaseSym],
    Set.empty[Symbol.RestrictableCaseSym],
    Set.empty[Symbol.StructFieldSym],
    Set.empty[Symbol.TraitSym],
    Set.empty[Symbol.SigSym],
    Set.empty[Symbol.LabelSym],
    Set.empty[Symbol.TypeAliasSym],
    Set.empty[Symbol.AssocTypeSym],
    Set.empty[Symbol.EffSym],
    Set.empty[Symbol.OpSym],
    Set.empty[Symbol.RegionSym],
    Set.empty[Symbol.ModuleSym]
  )
  def getSymbolSet(ty : TypeConstructor): SymbolSet = {
    val sset = emptySet
    ty match {
      case TypeConstructor.Enum(sym, _) => sset.addSymbol(sym)
      case TypeConstructor.Struct(sym, _) => sset.addSymbol(sym)
      case TypeConstructor.RestrictableEnum(sym, _) => sset.addSymbol(sym)
      case TypeConstructor.Case(sym) => sset.addSymbol(sym)
      case TypeConstructor.RestrictableCase(sym) => sset.addSymbol(sym)
      case TypeConstructor.StructField(sym) => sset.addSymbol(sym)
      case TypeConstructor.Trait(sym) => sset.addSymbol(sym)
      case TypeConstructor.Sig(sym) => sset.addSymbol(sym)
      case TypeConstructor.Label(sym) => sset.addSymbol(sym)
      case TypeConstructor.TypeAlias(sym) => sset.addSymbol(sym)
      case TypeConstructor.AssocType(sym) => sset.addSymbol(sym)
      case TypeConstructor.Effect(sym) => sset.addSymbol(sym)
      case TypeConstructor.Op(sym) => sset.addSymbol(sym)
      case TypeConstructor.RegionSym(sym) => sset.addSymbol(sym)
      case TypeConstructor.ModuleSym(sym) => sset.addSymbol(sym)
      case _ => sset
    }
  }
  def collectSymbols: Set[Symbol] = {
    enums ++
      structs ++
      restrictableEnum ++
      cases ++
      restrictableCases ++
      structField ++
      traits ++
      sigs ++
      labels ++
      typeAliases ++
      assocTypes ++
      effects ++
      ops ++
      regionSym ++
      modules
  }
  def findSymbol(search: Symbol => Boolean): Set[Symbol] = {
    collectSymbols.filter(search)
  }

}
