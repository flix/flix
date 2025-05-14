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
 

/**
Purpose: We need to make more predicates (at most 3 times as many) than there are relations.
Solution: The first n are the full relations, the next n are the delta relations, the last n are the temporary delta' relations.
Problem: Assuming that the relations are currently named 0 to n seems restrictive
Solution: Start by creating a map from old relation-numbers to new.
    Advantage: If we were to change this later we would only have to change the module Predicates. All other code does not need to care for the implementation.
 */

// Purpose: We need to make more predicates than there are 
mod Fixpoint.Predicates {
    use Fixpoint.Predicates.idToPredType
    use Fixpoint.Predicates.allFullRelSyms
    use Fixpoint.Predicates.PredType
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Ast.Shared.PredSym.PredSym
    use Fixpoint.Ast.Datalog.Datalog.Datalog
    use Fixpoint.Ast.Datalog.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadPredicate, HeadTerm, Polarity, VarSym}
    use Fixpoint.Ast.Datalog.Constraint.Constraint
    use Fixpoint.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint.Ast.Datalog.BodyPredicate.{BodyAtom}
    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RelSym, RamTerm, BoolExp, RowVar, Facts, Predicates, RamProgram, toId}
    use Fixpoint.Boxed

    @Internal
    pub enum PredType with Eq {
        case Full,
        case Delta,
        case New
    }

    @Internal
    pub def initialize(program: Datalog, db: Map[RelSym, Map[Vector[Boxed], Boxed]]): Predicates = 
        collectPreds(program, db)
        // let backMap = Map.mapWithKey(id -> v -> 
        //     match v {
        //         case RelSym.Symbol(PredSym(name, _), _, _) => PredSym(name, id)
        //     }
        // , forwardMap);
        // (max, forwardMap, backMap)
    
    @Internal
    pub def allFullRelSyms(predInfo: Predicates): List[RelSym] =
        let (_, set) = predInfo;
        Set.foldLeft(acc -> relSym -> 
            relSym :: acc
        , Nil, set)

    @Internal
    pub def allRelSyms(predTrack: Predicates): List[RelSym] =
        let full = allFullRelSyms(predTrack);
        full |>
        List.append(List.map(fullRelSymToType(predTrack, PredType.Delta), full)) |>
        List.append(List.map(fullRelSymToType(predTrack, PredType.New), full))

    pub def relSymsOfProgram(program: RamProgram): List[RelSym] = match program {
        case RamProgram.Program(_, _, predInfo, _) => allRelSyms(predInfo)
    }

    pub def isFullID(self: Predicates, id: Int64): Bool = 
        PredType.Full == idToPredType(id, self)

    pub def fullRelSymToType(self: Predicates, wantedType: PredType, relSym: RelSym): RelSym = match relSym {
        case RelSym.Symbol(PredSym.PredSym(name, id), arity, den) => RelSym.Symbol(PredSym.PredSym(name, fullIDtoPredType(id, wantedType, self)), arity, den)
    }

    pub def idToPredType(id: Int64, predInfo: Predicates): PredType = 
        let (max, _) = predInfo;
        if(id <= max) {
            PredType.Full
        } else if (id <= max * 2i64) {
            PredType.Delta
        } else {
            PredType.New                
        }

    // Could use previos method, but would duplicate computation... 
    pub def idToFullPredID(id: Int64, predInfo: Predicates): Int64 =
        let (max, _) = predInfo;
        if(id <= max) {
            id
        } else if (id <= max * 2i64) {
            id - max
        } else {
            id - 2i64 * max
        }

    pub def relSymToPredType(relSym: RelSym, predInfo: Predicates): PredType = match relSym {
        case RelSym.Symbol(PredSym.PredSym(_, id), _, _) => idToPredType(id, predInfo)
    }

    pub def relSymFromPredType(relSym: RelSym, wanted: PredType, self: Predicates): RelSym = match relSym {
        case RelSym.Symbol(PredSym.PredSym(name, id), arity, den) =>
            RelSym.Symbol(PredSym.PredSym(name, fullIDtoPredType(idToFullPredID(id, self), wanted, self)), arity, den)
    }

    // Could use previos method, but would duplicate computation... 
    pub def fullIDtoPredType(id: Int64, wantedType: PredType, self: Predicates): Int64 =
        let (max, _) = self;
        match wantedType {
            case PredType.Full => id
            case PredType.Delta => id + max
            case PredType.New => id + max * 2i64
        }

    ///
    /// Collect all predicate symbols in a set.
    ///
    def collectPreds(program: Datalog, db: Map[RelSym, Map[Vector[Boxed], Boxed]]): (Int64, Set[RelSym]) = match program {
        case Datalog(facts, rules) =>
            let set1 = collectPredicates(rules);
            let set2 = collectPredicates(facts);
            let set3 = collectDbPredicates(db);
            let set = (set1 `Set.union` set2) `Set.union` set3;
            let optMax = set |> Set.maximumBy(match RelSym.Symbol(PredSym.PredSym(_, id1), _, _) -> match RelSym.Symbol(PredSym.PredSym(_, id2), _, _) -> id1 <=> id2);
            match optMax {
                case None => (-1i64, set)
                case Some(max) => (toId(max) + 1i64, set)
            }
        case _ => bug!("Datalog Boxing bug")
    }

    def collectDbPredicates(db: Map[RelSym, Map[Vector[Boxed], Boxed]]): Set[RelSym] =
        Map.foldLeftWithKey(set -> relSym -> _ -> {
            Set.insert(relSym, set)
        }, Set#{}, db)

    def collectPredicates(constraints: Vector[Constraint]): Set[RelSym] =
        Vector.foldLeft(match set -> match Constraint(headAtom, body) -> {
            let setWithHead = Set.insert(headAtomToFullRelSym(headAtom), set);
            Vector.foldLeft(setBody -> bodyConstraint -> 
                match bodyConstraint {
                    case BodyAtom(_, _, _, _, _) => Set.insert(bodyAtomToFullRelSym(bodyConstraint), setBody)
                    case _ => setBody
                }, setWithHead, body)
        }, Set#{}, constraints)

    def headAtomToFullRelSym(head: HeadPredicate): RelSym = match head {
        case HeadAtom(pred, den, terms) => RelSym.Symbol(pred, Vector.length(terms), den)
    }

    def bodyAtomToFullRelSym(body: BodyPredicate): RelSym = match body {
        case BodyAtom(pred, den, _, _, terms) => RelSym.Symbol(pred, Vector.length(terms), den)
        case _ => unreachable!()
    }

    pub def headAtomToRelSym(head: HeadPredicate, t: PredType, predInfo: Predicates): RelSym =
        let fullRel = headAtomToFullRelSym(head);
        getRelSymAsType(fullRel, t, predInfo)

    pub def bodyAtomToRelSym(body: BodyPredicate, t: PredType, predInfo: Predicates): RelSym = 
        let fullRel = bodyAtomToFullRelSym(body);
        getRelSymAsType(fullRel, t, predInfo)

    pub def getRelSymAsType(relSym: RelSym, t: PredType, predInfo: Predicates): RelSym = match relSym {
        case RelSym.Symbol(pred, arity, den) => RelSym.Symbol(getPredSymAsType(pred, t, predInfo), arity, den)
    }

    pub def getPredSymAsType(predSym: PredSym, t: PredType, predInfo: Predicates): PredSym = match predSym {
        case PredSym.PredSym(name, id) => PredSym.PredSym(name, getIdAsType(id, t, predInfo))
    }

    pub def getIdAsType(id: Int64, t: PredType, predInfo: Predicates): Int64 =
        let (max, _) = predInfo;
        let fullId = idToFullPredID(id, predInfo);
        match t {
            case PredType.Full => fullId
            case PredType.Delta => fullId + max
            case PredType.New => fullId + 2i64 * max
    }
}
