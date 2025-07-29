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
/// This file provides an interface for interacting with `Fixpoint.Ast.Ram.Predicates`.
/// Core features include providing a list of all `RelSym` in `Predicates`,
/// provide the `PredType` of a `RelSym` and transforming `RelSym` of one
/// `PredType` into a `RelSym` of another `PredType`.
///
/// The three `PredType`s originate from semi-naive evaluation and are `Full`, `Delta` and `New`
/// (delta').
///
/// Instead of adding an enum type (Full/Delta/New) to the predicate, we embed the type into
/// the identifier, which we can do since we have an upper bound on the integers used.
///
/// For a `Datalog` AST consisting of predicates `PredSym("Road", 0)` and `PredSym("Path", 1)`
/// the RAM has 6 predicates, 3 for each predicate in the `Datalog` AST. Every predicate
/// has a `Full` version, a `Delta` version and a `New` version.
///
/// For `PredSym("Road", 0)` the predicates in RAM are `PredSym("Road", 0)`, `PredSym("Road", 2)`,
/// and `PredSym("Road", 4)`. `PredSym("Road", 0)` is the full predicate, `PredSym("Road", 2)`
/// is the delta predicate and `PredSym("Road", 4)` is the delta' predicate.
/// For `PredSym("Path", 1)` the predicates in RAM are be `PredSym("Path", 1)`, `PredSym("Path", 3)`,
/// and `PredSym("Path", 5)`.
///
/// In the above way `PredType` is encoded in the identifier of a `PredSym`.
///
mod Fixpoint3.Predicate {
    use Fixpoint3.Ast.Shared.PredSym
    use Fixpoint3.Ast.Shared.PredSym.PredSym
    use Fixpoint3.Ast.Datalog.Datalog.Datalog
    use Fixpoint3.Ast.Datalog.{BodyPredicate, BodyTerm, Constraint, Datalog, HeadPredicate, HeadTerm, Polarity, VarSym}
    use Fixpoint3.Ast.Datalog.Constraint.Constraint
    use Fixpoint3.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint3.Ast.Datalog.BodyPredicate.{BodyAtom}
    use Fixpoint3.Ast.Ram.{BoolExp, Facts, Predicates, RamProgram, RamStmt, RamTerm, RelOp, RelSym, RowVar, toId}
    use Fixpoint3.Boxed

    ///
    /// `PredType` describes the three types of predicates that exists in a RAM program.
    ///
    /// `Full` relations contains all facts computed for a relation.
    ///
    /// `Delta` relations contains, in iteration `i`, all facts computed in iteration `i-1` in fixpoints computations.
    ///
    /// `New` relations contains, in iteration `i`, all facts computed in iteration `i` in fixpoints computations.
    ///
    @Internal
    pub enum PredType with Eq {
        case Full,
        case Delta,
        case New
    }

    ///
    /// Returns a list of all full `RelSym` in `predInfo`.
    ///
    @Internal
    pub def allFullRelSyms(predInfo: Predicates): List[RelSym] =
        let (_, set) = predInfo;
        Set.foldLeft(acc -> relSym ->
            relSym :: acc
        , Nil, set)

    ///
    /// Returns a list of all `RelSym` in `predInfo`.
    ///
    @Internal
    pub def allRelSyms(predInfo: Predicates): List[RelSym] =
        let full = allFullRelSyms(predInfo);
        full |>
        List.append(List.map(x -> fullRelSymToType(x, PredType.Delta, predInfo), full)) |>
        List.append(List.map(x -> fullRelSymToType(x, PredType.New, predInfo), full))

    ///
    /// Returns the full `RelSym` associated with the `PredSym` in `body` according to `predInfo`.
    ///
    @Internal
    pub def bodyAtomToRelSym(body: BodyPredicate, wantedType: PredType, predInfo: Predicates): RelSym =
        let fullRel = bodyAtomToFullRelSym(body);
        getRelSymAsType(fullRel, wantedType, predInfo)

    ///
    /// Returns the identifier of `wantedType` of `id` according to `predInfo`
    ///
    @Internal
    pub def fullIdtoPredType(id: Int64, wantedType: PredType, predInfo: Predicates): Int64 =
        let (max, _) = predInfo;
        match wantedType {
            case PredType.Full => id
            case PredType.Delta => id + max
            case PredType.New => id + max * 2i64
        }

    ///
    /// Returns a `RelSym` of type `wantedType` given the `RelSym` `relSym`, which must be
    /// a full `RelSym`.
    ///
    @Internal
    pub def fullRelSymToType(relSym: RelSym, wantedType: PredType, predInfo: Predicates): RelSym = match relSym {
        case RelSym.Symbol(PredSym.PredSym(name, id), arity, den) => RelSym.Symbol(PredSym.PredSym(name, fullIdtoPredType(id, wantedType, predInfo)), arity, den)
    }

    ///
    /// Returns the identifier of type `wantedType` from `id` according to `predInfo`.
    ///
    @Internal
    pub def getIdAsType(id: Int64, wantedType: PredType, predInfo: Predicates): Int64 =
        let fullId = idToFullPredId(id, predInfo);
        fullIdtoPredType(fullId, wantedType, predInfo)

    ///
    /// Returns the `PredSym` of type `wantedType` from `predSym` according to `predInfo`.
    ///
    @Internal
    pub def getPredSymAsType(predSym: PredSym, wantedType: PredType, predInfo: Predicates): PredSym = match predSym {
        case PredSym.PredSym(name, id) => PredSym.PredSym(name, getIdAsType(id, wantedType, predInfo))
    }

    ///
    /// Returns the `RelSym` of type `wantedType` from `relSym` according to `predInfo`.
    ///
    @Internal
    pub def getRelSymAsType(relSym: RelSym, wantedType: PredType, predInfo: Predicates): RelSym = match relSym {
        case RelSym.Symbol(pred, arity, den) => RelSym.Symbol(getPredSymAsType(pred, wantedType, predInfo), arity, den)
    }

    ///
    /// Returns the full `RelSym` associated with the `PredSym` in `head` according to `predInfo`.
    ///
    @Internal
    pub def headAtomToRelSym(head: HeadPredicate, wantedType: PredType, predInfo: Predicates): RelSym =
        let fullRel = headAtomToFullRelSym(head);
        getRelSymAsType(fullRel, wantedType, predInfo)

    ///
    /// Returns the identifier of the full `PredSym` associated with the identifier `id`
    /// according to `predInfo`.
    ///
    @Internal
    pub def idToFullPredId(id: Int64, predInfo: Predicates): Int64 =
        let (max, _) = predInfo;
        if(id <= max) {
            id
        } else if (id <= max * 2i64) {
            id - max
        } else {
            id - 2i64 * max
        }

    ///
    /// Returns the type of a `PredSym` with identifier `id` according to `predInfo`.
    ///
    @Internal
    pub def idToPredType(id: Int64, predInfo: Predicates): PredType =
        let (max, _) = predInfo;
        if(id <= max) {
            PredType.Full
        } else if (id <= max * 2i64) {
            PredType.Delta
        } else {
            PredType.New
        }

    ///
    /// Collect all preciate information given a Datalog program `program` and database `db`.
    ///
    @Internal
    pub def initialize(program: Datalog, db: Map[RelSym, Map[Vector[Boxed], Boxed]]): Predicates =
        collectPreds(program, db)

    ///
    /// Returns true if `id` is the id of a full `PredSym` according to `predInfo`.
    ///
    @Internal
    pub def isFullId(id: Int64, predInfo: Predicates): Bool =
        PredType.Full == idToPredType(id, predInfo)

    ///
    /// Returns a `RelSym` of type `wantedType` from `relSym` according to `predInfo`.
    ///
    @Internal
    pub def relSymFromPredType(relSym: RelSym, wantedType: PredType, predInfo: Predicates): RelSym = match relSym {
        case RelSym.Symbol(PredSym.PredSym(name, id), arity, den) =>
            RelSym.Symbol(PredSym.PredSym(name, fullIdtoPredType(idToFullPredId(id, predInfo), wantedType, predInfo)), arity, den)
    }

    ///
    /// Returns a list of all `RelSym` known in `program`.
    ///
    @Internal
    pub def relSymsOfProgram(program: RamProgram): List[RelSym] = match program {
        case RamProgram.Program(_, _, predInfo, _) => allRelSyms(predInfo)
    }

    ///
    /// Returns the type of the `PredSym` in `relSym` according to `predInfo`.
    ///
    @Internal
    pub def relSymToPredType(relSym: RelSym, predInfo: Predicates): PredType = match relSym {
        case RelSym.Symbol(PredSym.PredSym(_, id), _, _) => idToPredType(id, predInfo)
    }

    ///
    /// Returns a set of all full `RelSym` created from `PredSym`s in `program` and `db` and the maximal identifier+1.
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

    ///
    /// Returns a set of full `RelSym` in `db`.
    ///
    def collectDbPredicates(db: Map[RelSym, Map[Vector[Boxed], Boxed]]): Set[RelSym] =
        Map.foldLeftWithKey(set -> relSym -> _ -> {
            Set.insert(relSym, set)
        }, Set#{}, db)

    ///
    /// Returns a set of full `RelSym` in `constraints`.
    ///
    def collectPredicates(constraints: Vector[Constraint]): Set[RelSym] =
        Vector.foldLeft(match set -> match Constraint(headAtom, body) -> {
            let setWithHead = Set.insert(headAtomToFullRelSym(headAtom), set);
            Vector.foldLeft(setBody -> bodyConstraint ->
                match bodyConstraint {
                    case BodyAtom(_, _, _, _, _) => Set.insert(bodyAtomToFullRelSym(bodyConstraint), setBody)
                    case _ => setBody
                }, setWithHead, body)
        }, Set#{}, constraints)

    ///
    /// Returns the full `RelSym` associated with the `PredSym` in `head`.
    ///
    def headAtomToFullRelSym(head: HeadPredicate): RelSym = match head {
        case HeadAtom(pred, den, terms) => RelSym.Symbol(pred, Vector.length(terms), den)
    }

    ///
    /// Returns the full `RelSym` associated with the `PredSym` in `body`.
    ///
    def bodyAtomToFullRelSym(body: BodyPredicate): RelSym = match body {
        case BodyAtom(pred, den, _, _, terms) => RelSym.Symbol(pred, Vector.length(terms), den)
        case _ => unreachable!()
    }

}
