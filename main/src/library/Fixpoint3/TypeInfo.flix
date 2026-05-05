/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

///
/// This module defines and offers an interface for dealing with the types of the Datalog engine.
///
/// A `Type` is uniquely defined by its `UnifiedTypePos` which is computed by `typeDatalog`.
///
/// This module has a tight coupling with `Compiler` since we must assign functional predicates
/// their identifiers in same order. It also has a tight coupling with `Lowering`, which assumes
/// that a `Type` is the same as a `UnifiedTypePoss`.
///
pub mod Fixpoint3.TypeInfo {
    use Fixpoint3.Ast.Datalog.{BodyPredicate, BodyTerm, Datalog, Constraint, HeadPredicate, HeadTerm, VarSym}
    use Fixpoint3.Ast.Ram.{RamProgram, RelSym, Predicates}
    use Fixpoint3.Ast.Shared.PredSym
    use Fixpoint3.Ast.Shared.PredSym.PredSym
    use Fixpoint3.Boxed
    use Fixpoint3.BoxingType.UnifiedTypePos
    use Fixpoint3.Counter
    use Fixpoint3.Predicate.{allFullRelSyms, getMax}
    use Fixpoint3.Util.getOrCrash

    ///
    /// As far as we are concerned a `Type` is simply a description of where
    /// its boxing/unboxing information is saved.
    ///
    pub type alias Type = UnifiedTypePos

    ///
    /// The type of a relation is simply a vector of its types.
    ///
    pub type alias RelType = Vector[Type]

    ///
    /// The type of a program is a vector where position `i` stores the type of
    /// the relation with predicate `PredSym(_, i)`.
    ///
    /// For functional predicates the vector contains the types of the
    /// functional predicates in the same order they are met by the compiler.
    ///
    pub type alias TypeInformation = Vector[RelType]

    type alias TypeUnifier[r: Region] = MutDisjointSets[(Int64, Int32), r]

    type alias VarSymUnifier[r: Region] = MutMap[String, (Int64, Int32), r]

    type alias PseudoStrataOrder[r: Region] = MutMap[Int32, MutMap[Int32, List[(Int32, Constraint)], r], r]

    ///
    /// Cast `i` to an `Int64` and crash if `i` does not fit.
    ///
    def toInt32(i: Int64): Int32 = getOrCrash(Int64.tryToInt32(i))

    ///
    /// Returns the type of provenance annotations.
    ///
    pub def provType(): Type = 0

    ///
    /// Returns the type information of `d`
    ///
    /// For functional predicates the vector returned vector contains the types of the
    /// functional predicates in the same order they are met by the compiler.
    ///
    pub def typeDatalog(d: Datalog, pseudoStratums: PseudoStrataOrder[r], predicates: Predicates, withProv: Bool): TypeInformation \ r = match d {
        case Datalog.Datalog(_, _) => region rc {
            let idCreator = Counter.fresh(rc);
            // Ensure that the first index is 1.
            let rels = allFullRelSyms(predicates);
            let relNum = getOrCrash(Int64.tryToInt32(getMax(predicates)));
            // The first functional will get an id 1 greater than the largest PredSym.
            Counter.set(relNum, idCreator);
            let typeUnifier = MutDisjointSets.empty(rc);
            // Have to traverse in same order as `Compiler.flix`
            foreach ((_, pseudStrat) <- pseudoStratums) {
                MutMap.foldLeft(_ -> rules2 -> {
                    List.forEach(match (_, rule) -> typeConstraint(rule, typeUnifier, idCreator), rules2)
                }, (), pseudStrat)
            };

            let maxId = Counter.get(idCreator);
            let arr = Array.empty(rc, maxId);
            // Register `RelSym`s.
            foreach(RelSym.Symbol(PredSym(_, id), arity, _) <- rels) {
                Array.put(Array.repeat(rc, arity, -1), toInt32(id), arr)
            };
            // Register functional predicates.
            foreach (id <- Vector.range(relNum, maxId)) {
                let arity = computeSizeOfFunctional(Int32.toInt64(id), 0, typeUnifier);
                Array.put(Array.repeat(rc, arity, -1), id, arr)
            };

            let typeCreator = Counter.fresh(rc);
            if (withProv) {
                // Reserve position 0 for provenance values.
                Counter.increment(typeCreator)
            } else {
                ()
            };
            foreach (id <- Vector.range(0, maxId)) {
                let curArr = Array.get(id, arr);
                foreach (index <- Vector.range(0, Array.length(curArr))) {
                    match MutDisjointSets.find((Int32.toInt64(id), index), typeUnifier) {
                        case None => Array.put(Counter.getAndIncrement(typeCreator), index, curArr)
                        case Some((id2Int64, index2)) =>
                            let id2 = toInt32(id2Int64);
                            let otherArr = Array.get(id2, arr);
                            match Array.get(index2, otherArr) {
                                case -1 =>
                                    let newType = Counter.getAndIncrement(typeCreator);
                                    Array.put(newType, index2, otherArr);
                                    Array.put(newType, index, curArr)
                                case someType =>
                                    Array.put(someType, index, curArr)
                            }
                    }
                }
            };
            // Don't copy arrays, just cast them.
            unchecked_cast(arr as TypeInformation)
        }
        case _ => unreachable!()
    }

    ///
    /// Expands `typeInfo` to include the provenance annotations types.
    ///
    pub def expandTypeInfoToProv(typeInfo: TypeInformation, predicates: Predicates): TypeInformation =
        let max = getMax(predicates);
        Vector.mapWithIndex(i -> v -> match Int32.toInt64(i) < max {
            case true => expandTypeToProv(v)
            case false => v
        }, typeInfo)


    ///
    /// Expands `relType` to include the provenance annotations types.
    ///
    pub def expandTypeToProv(relType: RelType): RelType = relType ++ Vector#{provType(), provType()}

    ///
    /// Returns the size of the output of the functional with assigned identifier `id`.
    ///
    def computeSizeOfFunctional(id: Int64, count: Int32, typeUnifier: TypeUnifier[r]): Int32 \ r =
        match MutDisjointSets.find((id, count), typeUnifier) {
            case None => count
            case Some(_) => computeSizeOfFunctional(id, count + 1, typeUnifier)
        }

    ///
    /// Returns the type of the `index`'th value of the relation with predicate `PredSym(_, id)`.
    ///
    pub def getType(id: Int64, index: Int32, typeInfo: TypeInformation): Type = {
        Vector.get(index, getTypeOf(id, typeInfo))
    }

    ///
    /// Returns the type of the relation with predicate `PredSym(_, id)`.
    ///
    pub def getTypeOf(id: Int64, typeInfo: TypeInformation): RelType = {
        Vector.get(toInt32(id), typeInfo)
    }

    ///
    /// Unifies the types of `rule`.
    ///
    def typeConstraint(rule: Constraint, typeUnifier: TypeUnifier[r], counter: Counter[r]): Unit \ r = match rule {
        case Constraint.Constraint(head, body) => region rc {
            let varSymUnifier = MutMap.empty(rc);
            typeHead(head, varSymUnifier, typeUnifier);
            Vector.forEach(bodyPred -> typeBodyPred(bodyPred, varSymUnifier, typeUnifier, counter), body)
        }
    }

    ///
    /// Unifies the types of `head`.
    ///
    def typeHead(head: HeadPredicate, varSymUnifier: VarSymUnifier[r1], typeUnifier: TypeUnifier[r2]): Unit \ r1 + r2 =
        let HeadPredicate.HeadAtom(PredSym(_, id), _, terms) = head;
        foreach((i, t) <- ForEach.withIndex(terms)) {
            match t {
                case HeadTerm.Var(VarSym.VarSym(sym)) =>
                    typeVar(id, i, sym, varSymUnifier, typeUnifier)
                case _ => ()
            }
        }

    ///
    /// Unifies the types of `bodyPred`.
    ///
    def typeBodyPred(bodyPred: BodyPredicate, varSymUnifier: VarSymUnifier[r1], typeUnifier: TypeUnifier[r2], counter: Counter[r2]): Unit \ r1 + r2 = match bodyPred {
        case BodyPredicate.BodyAtom(PredSym(_, id), _, _, _, terms) =>
            Vector.forEachWithIndex(index -> term -> typeBodyTerm(id, index, term, varSymUnifier, typeUnifier), terms)
        case BodyPredicate.Functional(outVars, _, _inVars) =>
            let id = Int32.toInt64(Counter.getAndIncrement(counter));
            Vector.forEachWithIndex(index -> match VarSym.VarSym(sym) -> typeVar(id, index, sym, varSymUnifier, typeUnifier), outVars)
        case _ => ()
    }

    ///
    /// Register that `(id, index) == sym`.
    ///
    def typeVar(id: Int64, index: Int32, sym: String, varSymUnifier: VarSymUnifier[r1], typeUnifier: TypeUnifier[r2]): Unit \ r1 + r2 = {
        let (repId, repIndex) = MutMap.getOrElsePut(sym, (id, index), varSymUnifier);
        if (id == repId and repIndex == index) {
            MutDisjointSets.makeSet((repId, repIndex), typeUnifier)
        } else {
            MutDisjointSets.union((repId, repIndex), (id, index), typeUnifier)
        }
    }

    ///
    /// Register that `(id, index) == term`.
    ///
    def typeBodyTerm(id: Int64, index: Int32, term: BodyTerm, varSymUnifier: VarSymUnifier[r1], typeUnifier: TypeUnifier[r2]): Unit \ r1 + r2 = match term {
        case BodyTerm.Wild => ()
        case BodyTerm.Lit(_) => ()
        case BodyTerm.Var(VarSym.VarSym(sym)) => typeVar(id, index, sym, varSymUnifier, typeUnifier)
    }

}
