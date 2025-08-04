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
/// This module reconstructs the proof tree given a provenance model.
/// It does so by re-evaluating the program in a top-down fashion
/// to prove the sought after atom. This search is augmented with
/// knowledge about which facts are true and the rules used to
/// construct them.
///
/// The current implementation assumes that all negative atoms are placed after positive atoms.
/// More precicely it assumes that all variables in the negative atoms have
/// been bound when it encounters them.
///
/// The current implementation does not output the negative atoms. They can be derived from the positive.
///
mod Fixpoint3.Provenance {
    use Fixpoint3.Ast.Datalog.{Datalog, Constraint, VarSym, BodyPredicate, HeadPredicate, HeadTerm, BodyTerm, Polarity}
    use Fixpoint3.Ast.Datalog.Datalog.{Datalog, Model, Join, Provenance}
    use Fixpoint3.Ast.Ram.{makeFakeRelSym, RelSym}
    use Fixpoint3.Ast.Shared.{PredSym, Denotation}
    use Fixpoint3.Boxed
    use Fixpoint3.Counter
    use Fixpoint3.Util.getOrCrash

    ///
    /// A `ProvIDB` maps from `PredSym` to a map from the facts of the `PredSym`
    /// to `(depth, ruleUsed)`.
    ///
    type alias ProvIDB = Map[RelSym, BPlusTree[Vector[Boxed], (Int64, Int32), Static]]

    ///
    /// A `BPLookoup` is a over the facts for a specific `PredSym`.
    ///
    /// For a fact set of `(1, 2, 3), (4, 5, 6), (7, 8, 9)`, we might want
    /// to index based on the second attribute. The `BPLookup` would be
    /// `{[2] => [1, 3], [5] => [4, 6], [8] => [7, 9]}`.
    ///
    type alias BPLookup[r: Region] = BPlusTree[Vector[Boxed], List[(Vector[Boxed], Int64, Int32)], r]

    ///
    /// A mutable map from `PredSym` to a `MutMap` from the the wanted lookup-slice to
    /// a `BPLookup` on said slice.
    ///
    /// Using the previous example with predicate `P` we would have
    /// `{P => {[1] => [2] => [1, 3], [5] => [4, 6], [8] => [7, 9]}}`
    ///
    type alias ProvLookupStruct[r: Region] = MutMap[PredSym, MutMap[Vector[Int32], BPLookup[r], r], r]

    ///
    /// Returns the list of facts for `pred` which satisfies `searchAndVals`.
    ///
    /// If `searchAndVals = [(0, BoxedInt32(2)), (3, BoxedInt32(3))]` this should be
    /// understood as the first value of the fact of `pred` should be `BoxedInt32(2)`
    /// and the fourth should be `BoxedInt32(3)`.
    ///
    /// Returns `None` if no values match.
    ///
    /// Returns `Some(v)` where `v` is a list of `(missingFactPart, depth, rule)`. Here
    /// `missingFactPart` refers to the value-part in the `BPLookup`.
    ///
    def lookup(rc: Region[r], idb: ProvIDB, searchAndVals: Vector[(Int32, Boxed)], arrSize: Int32, lookupStruct: ProvLookupStruct[r], pred: PredSym): Option[List[(Vector[Boxed], Int64, Int32)]] \ r + IO =
        match Map.get(makeFakeRelSym(pred), idb) {
            case None => None
            case Some(fullTree) => 
                let (search, vals) = Vector.unzip(searchAndVals);
                let innerMap = MutMap.getOrElsePut(pred, MutMap.empty(rc), lookupStruct);
                match MutMap.get(search, innerMap) {
                    case Some(tree) => BPlusTree.get(vals, tree)
                    case None =>
                        // We have never encountered this search before. Initialize it.
                        let tree = BPlusTree.emptyWithSearch(rc, search);
                        let extractSearchPart = k -> Vector.init(i -> Vector.get(Vector.get(i, search), k), Vector.length(search));
                        let nonSearch = Vector.range(0, arrSize) |> Vector.filter(v -> not Vector.memberOf(v, search));
                        let extractNonSearchPart = k -> Vector.init(i -> Vector.get(Vector.get(i, nonSearch), k), Vector.length(nonSearch));
                        fullTree |>
                            BPlusTree.forEach(k -> match (depth, rule) -> {
                                let searchPart = extractSearchPart(k);
                                let nonSearchPart = extractNonSearchPart(k);
                                let newVal = ((nonSearchPart), depth, rule);
                                BPlusTree.putWith(_ -> old -> newVal :: old, searchPart, newVal :: Nil, tree)
                            });
                        MutMap.put(search, tree, innerMap);
                        BPlusTree.get(vals, tree)
                }
        }

    ///
    /// A proof tree of an atom.
    ///
    /// `EDB(P, tuple)` signifies that `P(tuple)` is a ground atom, with respect
    /// to `ProvIDB`.
    ///
    /// `Negative(P, tuple)` signifies that `P(tuple)` is not in the model, with
    /// respect to `ProvIDB`.
    ///
    /// `IDB(P, tuple, subproofs)` signifies that `P(tuple)` is in the model, with
    /// respect to `ProvIDB`, and it is not a ground atom. Furthermore `subproofs`
    /// contains the proofs/facts of the body which (could have) derived `P(tuple)`.
    ///
    enum ProofTree {
        case EDB(PredSym, Vector[Boxed])
        case Negative(PredSym, Vector[Boxed])
        case IDB(PredSym, Vector[Boxed], Vector[ProofTree])
    }

    mod ProofTree {
        @Internal
        pub def toString(p: ProofTree, indentChild: Int32): String = match p {
            case EDB(pred, fact) => 
                let indentCur = String.repeat(indentChild, " ");
                let factString = Vector.join(", ", fact);
                "${indentCur}${pred}(${factString}). Fact"
            case Negative(pred, fact) => 
                let indentCur = String.repeat(indentChild, " ");
                let factString = Vector.join(", ", fact);
                "${indentCur}No ${pred}(${factString})"
            case IDB(pred, fact, subs) => 
                let factString = Vector.join(", ", fact);
                let indentCur = String.repeat(indentChild, " ");
                let subProofs = Vector.joinWith(sub -> toString(sub, indentChild + 4), "${String.lineSeparator()}", subs);
                "${indentCur}${pred}(${factString}). Proof:${String.lineSeparator()}${subProofs}"
        }
    }

    instance ToString[ProofTree] {
        pub def toString(p: ProofTree): String = ProofTree.toString(p, 0)
    }

    ///
    /// Flattens the proof `p` to a `List`, removing facts that do not satisfy `filterFunc`.
    ///
    /// The returned `List` is in `Pre-order`.
    ///
    def filterAndFlattenProof(p: ProofTree, filterFunc: PredSym -> Bool): List[(PredSym, Vector[Boxed])] = region rc {
        let res = MutList.empty(rc);
        def recurse(workList: List[ProofTree]) = match workList {
            case Nil => ()
            case ProofTree.EDB(pred, fact) :: tail =>
                if (filterFunc(pred)) MutList.push((pred, fact), res)
                else ();
                recurse(tail)
            case ProofTree.Negative(_, _) :: tail => recurse(tail)
            case ProofTree.IDB(pred, fact, subTrees) :: tail => {
                if (filterFunc(pred)) MutList.push((pred, fact), res)
                else ();
                let furtherWork = (tail, subTrees) ||> Vector.foldLeft(acc -> cur -> cur :: acc);
                recurse(furtherWork)
        }};
        recurse(p :: Nil);
        MutList.toList(res)
    }

    @Internal
    pub def provQuery(predSym: PredSym, fact: Vector[Boxed], d: Datalog): Option[List[(PredSym, Vector[Boxed])]] =
        unsafely IO run provQueryInternal(predSym, fact, d)

    @Internal
    pub def provQueryInternal(predSym: PredSym, fact: Vector[Boxed], d: Datalog): Option[List[(PredSym, Vector[Boxed])]] \ IO = match d {
        case Provenance(rules, provIdb) => region rc {
            let reorderedRules = Vector.map(identity, rules);
            let annotation = match Map.get(RelSym.Symbol(predSym, -1, Denotation.Relational), provIdb) {
                case None => None
                case Some(v) => match BPlusTree.get(fact, v) {
                    case None => None
                    case Some(annotations) => Some(annotations)
                }
            };
            match annotation {
                case None => None
                case Some((depth, ruleUsed)) => 
                    let lookupStruct = MutMap.empty(rc);
                    let tree = buildProof(predSym, fact, depth, ruleUsed, reorderedRules, provIdb, lookupStruct, rc);
                    Some(filterAndFlattenProof(tree, _ -> true))
            }
        }
        case _ => 
            bug!("In Fixpoint.Provenance.provQuery: Can only build a proof from a provenance model")
    }

    def buildProof(predSym: PredSym, fact: Vector[Boxed], depth: Int64, ruleUsed: Int32, rules: Vector[Constraint], provIdb: ProvIDB, lookupStruct: ProvLookupStruct[r], rc: Region[r]): ProofTree \ r + IO = {
        if(ruleUsed == -1) {
            ProofTree.EDB(predSym, fact)
        } else if(ruleUsed == -2) {
            ProofTree.Negative(predSym, fact)
        } else {
            let rule = Vector.get(ruleUsed, rules);
            let bodyAtoms = getMatchingBody(fact, depth, rc, provIdb, lookupStruct, rule);
            let Constraint.Constraint(_, bodyPreds) = rule;
            let relevantPreds = bodyPreds |>
                Vector.filter(bodyAtom -> match bodyAtom {
                    case BodyPredicate.BodyAtom(_, _, _, _, _) => true
                    case _ => false
                });
            let childProofs = Vector.zip(relevantPreds, bodyAtoms) |>
                Vector.map(match (bodyP, (bodyFact, bodyDepth, bodyRule)) -> match bodyP {
                    case BodyPredicate.BodyAtom(p, _, _, _, _) =>
                        buildProof(p, bodyFact, bodyDepth, bodyRule, rules, provIdb, lookupStruct, rc)
                    case _      => unreachable!()
                });
            ProofTree.IDB(predSym, fact, childProofs)
        }
    }

    def getMatchingBody(
        fact: Vector[Boxed],
        depth: Int64,
        rc: Region[r],
        provIdb: ProvIDB,
        lookupStruct: ProvLookupStruct[r],
        rule: Constraint
    ): Vector[(Vector[Boxed], Int64, Int32)] \ r + IO = {
        let Constraint.Constraint(HeadPredicate.HeadAtom(_, _, headTerms), bodyPreds) = rule;
        let bodyValues = Array.init(rc, _ -> None, Vector.length(bodyPreds));
        let getValOfVar = MutMap.empty(rc);
        let headApps = Vector.zip(headTerms, fact) |>
            Vector.filter(match (h, _) -> match h {
                case (HeadTerm.App0(_)) => false // Must be true.
                case (HeadTerm.App1(_, _)) => true
                case (HeadTerm.App2(_, _, _)) => true
                case (HeadTerm.App3(_, _, _, _)) => true
                case (HeadTerm.App4(_, _, _, _, _)) => true
                case (HeadTerm.App5(_, _, _, _, _, _)) => true
                case _ => false
            });
        ///
        /// Returns the current value of `varName`.
        ///
        def getVal(varName) = getOrCrash(MutMap.get(varName, getValOfVar));

        ///
        /// Register the values new values of `varNames` to be `matchedFacts`.
        ///
        /// Concretely, we update the `getValOfVar` to contain `varNames[i] => matchedFacts[i]`.
        ///
        def putVals(varNames, matchedFact) =
            varNames |>
                Vector.forEachWithIndex(i -> v ->
                    MutMap.put(v, Vector.get(i, matchedFact), getValOfVar)
                );
        ///
        /// Returns true if all guards are satisfied by the current variable assignment.
        ///
        def areGuardsTrue(): Bool = {
            headApps |>
                Vector.forAll(match (app, val) -> match app {
                    case (HeadTerm.App1(f, v1))                 => val == f(getVal(v1))
                    case (HeadTerm.App2(f, v1, v2))             => val == f(getVal(v1), getVal(v2))
                    case (HeadTerm.App3(f, v1, v2, v3))         => val == f(getVal(v1), getVal(v2), getVal(v3))
                    case (HeadTerm.App4(f, v1, v2, v3, v4))     => val == f(getVal(v1), getVal(v2), getVal(v3), getVal(v4))
                    case (HeadTerm.App5(f, v1, v2, v3, v4, v5)) => val == f(getVal(v1), getVal(v2), getVal(v3), getVal(v4), getVal(v5))
                    case _ => unreachable!()
                })
        };
        
        ///
        /// Attempts to satisfy `bodyPreds`, having satisfied already found a satisfying assignment
        /// for `bodyPreds[i]`, where `0 <= i < index`.
        ///
        /// Destructively updates `getValOfVar` and `bodyValues` with newly bound values.
        ///
        /// Returns `true` if the current values in `getValOfVar` and `bodyValues` satisfies `rule`
        /// and `false` otherwise.
        ///
        def loop(index) = {
            if (index == Vector.length(bodyPreds)) {
                // TODO: Can be removed???
                areGuardsTrue()
            } else {
                // This could, for performance, be something that was done once and only once.
                // It would however require a new representation of a Datalog program.
                match Vector.get(index, bodyPreds) {
                    case BodyPredicate.BodyAtom(pred, _, Polarity.Positive, _, expected) =>
                        // Collect the bound variables in a list as needed by `lookup`.
                        // `(2, val)` being in the list means that attribute 2 of the tuple
                        // must have value `val`. View a a `zipWithIndex |> filterWithIndex`.
                        let filterAsList = ((Nil, 0), expected)
                            ||> Vector.foldLeft(match (acc, i) -> t -> match t {
                                case BodyTerm.Wild => (acc, i + 1)
                                case BodyTerm.Var(v) => match MutMap.get(v, getValOfVar) {
                                    case None => (acc, i + 1)
                                    case Some(val) => ((i, val) :: acc, i + 1)
                                }
                                case BodyTerm.Lit(val) => ((i, val) :: acc, i + 1)
                            }) |> fst |> List.toVector |> Vector.reverse;
                        // List of `VarSym` of variables that become bound when the atom is
                        // satisfied.
                        let nowBoundVars = expected |>
                            Vector.filterMap(t -> match t {
                                case BodyTerm.Var(v) => match MutMap.get(v, getValOfVar) {
                                    case None => Some(v)
                                    case Some(_) => None
                                }
                                case _ => None
                            });
                        let matches = lookup(rc, provIdb, filterAsList, Vector.length(expected), lookupStruct, pred);
                        match matches {
                            case None => false
                            case Some(facts) => {
                                // Try to find any fact that matches the current assignment of variables.
                                facts |>
                                    List.exists(match (bodyFact, bodyDepth, bodyRule) -> {
                                        if(bodyDepth >= depth) {
                                            false
                                        } else {
                                            let c1 = Counter.fresh(rc);
                                            let c2 = Counter.fresh(rc);
                                            let completeFact = Vector.range(0, Vector.length(expected)) |>
                                                Vector.map(i -> {
                                                    if(Counter.get(c1) < Vector.length(filterAsList) and i == fst(Vector.get(Counter.get(c1), filterAsList))) {
                                                        snd(Vector.get(Counter.getAndIncrement(c1), filterAsList))
                                                    } else {
                                                        Vector.get(Counter.getAndIncrement(c2), bodyFact)
                                                    }
                                                });
                                            Array.put(Some((completeFact, bodyDepth, bodyRule)), index, bodyValues);
                                            putVals(nowBoundVars, bodyFact);
                                            loop(index + 1)
                                        }
                                    })
                            }
                        }
                    case BodyPredicate.BodyAtom(pred, _, Polarity.Negative, _, expected) =>
                        // `not pred(tuple)` is true if `pred(tuple)` is not in the model. 
                        let tuple = expected |> 
                            Vector.map(t -> match t {
                                case BodyTerm.Var(v) => getVal(v)
                                case BodyTerm.Lit(val) => val
                                case BodyTerm.Wild => unreachable!()
                            });
                        let isNonMatch = match Map.get(makeFakeRelSym(pred), provIdb) {
                            case None => true
                            case Some(facts) => not BPlusTree.memberOf(tuple, facts)
                        };
                        if(not isNonMatch)
                            false
                        else {
                            // No need to save the match. Negative atoms cannot bind variables.
                            Array.put(Some((tuple, -2i64, -2)), index, bodyValues);
                            loop(index + 1)
                        }
                    case BodyPredicate.Guard0(_) =>
                        // No need to evaluate `f()`, it must be `true`, otherwise what we know is a fact
                        // would not have been created by this rule.
                        loop(index + 1)
                    case BodyPredicate.Guard1(f, v1) =>
                        if(f(getVal(v1)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard2(f, v1, v2) =>
                        if(f(getVal(v1), getVal(v2)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard3(f, v1, v2, v3) =>
                        if(f(getVal(v1), getVal(v2), getVal(v3)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard4(f, v1, v2, v3, v4) =>
                        if(f(getVal(v1), getVal(v2), getVal(v3), getVal(v4)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard5(f, v1, v2, v3, v4, v5) =>
                        if(f(getVal(v1), getVal(v2), getVal(v3), getVal(v4), getVal(v5)))
                            loop(index + 1)
                        else false
                    // Functional in provenance is not supported.
                    case BodyPredicate.Functional(_, _, _) => unreachable!()
                }
            }};
        headTerms |>
            Vector.mapWithIndex(i -> h -> match h {
                case HeadTerm.Var(v) => MutMap.put(v, Vector.get(i, fact), getValOfVar)
                case _ => ()
            });
        loop(0);
        bodyValues |> Array.filterMap(rc, identity) |> Array.toVector
    }
}
