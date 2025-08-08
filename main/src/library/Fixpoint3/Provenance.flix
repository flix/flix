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
/// been bound if a rule is evaluated left to right.
///
/// The current implementation assumes that guards satisfy the same property as the negative atoms,
/// namely that all their variables are bound when they are encountered when evaluating a rule
/// left to right.
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
    /// A `ProvIDB` maps from `RelSym` to a map from the facts of the `RelSym`
    /// to `(depth, ruleUsed)`.
    ///
    type alias ProvIDB = Map[RelSym, BPlusTree[Vector[Boxed], (Int64, Int32), Static]]

    ///
    /// A `BPLookup` is a over the facts for a specific `RelSym`.
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
    type alias ProvLookupStruct[r: Region] = BPlusTree[PredSym, BPlusTree[Vector[Int32], BPLookup[r], r], r]

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
    def lookup(rc: Region[r], idb: ProvIDB, searchAndVals: Vector[(Int32, Boxed)], arity: Int32, lookupStruct: ProvLookupStruct[r], pred: PredSym): Option[List[(Vector[Boxed], Int64, Int32)]] \ r + IO =
        match Map.get(makeFakeRelSym(pred), idb) {
            case None => unreachable!() // No facts for `pred`.
            case Some(fullTree) =>
                let (search, vals) = Vector.unzip(searchAndVals);
                let innerMap = BPlusTree.computeIfAbsent(() -> BPlusTree.empty(rc), pred, lookupStruct);
                match BPlusTree.get(search, innerMap) {
                    case Some(tree) => BPlusTree.get(vals, tree)
                    case None =>
                        // We have never encountered this search before. Initialize the `BPlusTree` for it.
                        // Initializing happens by mapping every fact to the search-part and non-search part
                        // and inserting them as key-value pairs in a `BPLookup`.
                        let tree = BPlusTree.emptyWithSearch(rc, search);
                        let extractSearchPart = k -> Vector.init(i -> Vector.get(Vector.get(i, search), k), Vector.length(search));
                        let nonSearch = Vector.range(0, arity) |> Vector.filter(v -> not Vector.memberOf(v, search));
                        let extractNonSearchPart = k -> Vector.init(i -> Vector.get(Vector.get(i, nonSearch), k), Vector.length(nonSearch));
                        foreach ((k, (depth, rule)) <- fullTree) {
                            let searchPart = extractSearchPart(k);
                            let nonSearchPart = extractNonSearchPart(k);
                            let newVal = (nonSearchPart, depth, rule);
                            BPlusTree.putWith(_ -> old -> newVal :: old, searchPart, newVal :: Nil, tree)
                        };
                        BPlusTree.put(search, tree, innerMap);
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
    /// The tree is traversed in a pre-order fashion meaning that for `IDB` atoms the atom is listed before the children.
    ///
    def filterAndFlattenProof(p: ProofTree, filter: PredSym -> Bool): Vector[(PredSym, Vector[Boxed])] = region rc {
        let res = MutList.empty(rc);
        def recurse(workList: List[ProofTree]) = match workList {
            case Nil => ()
            case ProofTree.EDB(pred, fact) :: tail =>
                if (filter(pred)) MutList.push((pred, fact), res)
                else ();
                recurse(tail)
            case ProofTree.Negative(_, _) :: tail => recurse(tail)
            case ProofTree.IDB(pred, fact, subTrees) :: tail => {
                if (filter(pred)) MutList.push((pred, fact), res)
                else ();
                let furtherWork = Vector.foldRight(cur -> acc -> cur :: acc, tail, subTrees);
                recurse(furtherWork)
        }};
        recurse(p :: Nil);
        MutList.toVector(res)
    }

    ///
    /// Returns `None` if `predSym(fact)` is not a part of the provenance model `d`.
    ///
    /// Returns `Some(v)` where `v` is a vector of the facts that can be used to prove `fact`.
    /// In other words they are the result of flattening the proof tree of `predSym(fact)`.
    ///
    /// Crashes if `d` is not a provenance model.
    ///
    @Internal
    pub def provQuery(predSym: PredSym, fact: Vector[Boxed], withh: Vector[PredSym], d: Datalog): Option[Vector[(PredSym, Vector[Boxed])]] =
        unsafely IO run provQueryInternal(predSym, fact, withh, d)

    ///
    /// Returns `None` if `predSym(fact)` is not a part of the provenance model `d`.
    ///
    /// Returns `Some(v)` where `v` is a vector of the facts that can be used to prove `fact`.
    /// In other words they are the result of flattening the proof tree of `predSym(fact)`.
    ///
    /// Crashes if `d` is not a provenance model.
    ///
    /// The `IO` effect is from reading from `Static` data structures.
    ///
    def provQueryInternal(predSym: PredSym, fact: Vector[Boxed], withh: Vector[PredSym], d: Datalog): Option[Vector[(PredSym, Vector[Boxed])]] \ IO = match d {
        case Provenance(rules, provIdb) => region rc {
            let annotation = match Map.get(makeFakeRelSym(predSym), provIdb) {
                case None => None
                case Some(v) => match BPlusTree.get(fact, v) {
                    case None => None
                    case Some(annotations) => Some(annotations)
                }
            };
            match annotation {
                case None => None
                case Some((depth, ruleUsed)) =>
                    let lookupStruct = BPlusTree.empty(rc);
                    let tree = buildProof(predSym, fact, depth, ruleUsed, rules, provIdb, lookupStruct, rc);
                    Some(filterAndFlattenProof(tree, p -> Vector.memberOf(p, withh)))
            }
        }
        case _ =>
            bug!("In Fixpoint.Provenance.provQuery: Can only build a proof from a provenance model")
    }

    ///
    /// Builds the provenance tree for `predSym(fact)` where `fact` was derived with
    /// depth `depth` using `rules[ruleUsed]`.
    ///
    /// `ruleUsed == -1` means that `predSym(fact)` is an `EDB` atom. `ruleUsed == -2`
    /// means that `predSym(fact)` is a negative fact/not a fact in the model.
    ///
    def buildProof(predSym: PredSym, fact: Vector[Boxed], depth: Int64, ruleUsed: Int32, rules: Vector[Constraint], provIdb: ProvIDB, lookupStruct: ProvLookupStruct[r], rc: Region[r]): ProofTree \ r + IO = {
        if (ruleUsed == -1) {
            ProofTree.EDB(predSym, fact)
        } else if (ruleUsed == -2) {
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
                    case _ => unreachable!()
                });
            ProofTree.IDB(predSym, fact, childProofs)
        }
    }

    ///
    /// Returns a vector of `(values, subDepth, subRule)` that satisfy `rule` and would imply `fact`.
    /// In the tuple `subDepth` is the depth at which `values` was derived and `subRule` the rule used
    /// to derive it.
    ///
    /// In other words return a vector of facts that could have derived `fact`,
    /// assuming `fact` was derived by `rule`.
    ///
    /// The returned facts will have a depth stricly smaller than `depth`.
    ///
    def getMatchingBody(
        fact: Vector[Boxed],
        depth: Int64,
        rc: Region[r],
        provIdb: ProvIDB,
        lookupStruct: ProvLookupStruct[r],
        rule: Constraint
    ): Vector[(Vector[Boxed], Int64, Int32)] \ r + IO = {
        let Constraint.Constraint(HeadPredicate.HeadAtom(_, _, headTerms), bodyPreds) = rule;
        // Array which contains the current assignment of body atoms.
        // Not all body predicates will be atoms. After termination of `loop` a
        // `None` at position `i` will be mean that `rule[i]` was not a body atom.
        let bodyValues: Array[Option[(Vector[Boxed], Int64, Int32)], r] = Array.repeat(rc, Vector.length(bodyPreds), None);
        // A mapping from `VarSym` to its current value.
        let getValOfVar: MutMap[VarSym, Boxed, r] = MutMap.empty(rc);
        let headApps = Vector.zip(headTerms, fact) |>
            Vector.filter(match (h, _) -> match h {
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
        def registerVars(varNames, matchedFact) =
            foreach ((i, v) <- ForEach.withIndex(varNames)) {
                MutMap.put(v, Vector.get(i, matchedFact), getValOfVar)
            };
        ///
        /// Returns true if all head applications are satisfied by the current variable assignment.
        ///
        def areHeadApplicationsSatisfied(): Bool = {
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
        /// This is a top down evaluation of `rule`.
        ///
        /// Attempts to satisfy `bodyPreds`, having already found a satisfying assignment
        /// for `bodyPreds[i]`, where `0 <= i < index`.
        ///
        /// Destructively updates `getValOfVar` and `bodyValues` with newly bound values.
        ///
        /// Returns `true` if the current values in `getValOfVar` and `bodyValues` satisfies `rule`
        /// and `false` otherwise.
        ///
        def loop(index) = {
            if (index == Vector.length(bodyPreds)) {
                // Base case. We have satisfied all atoms and found a satisfying assignment.
                areHeadApplicationsSatisfied()
            } else {
                match Vector.get(index, bodyPreds) {
                    case BodyPredicate.BodyAtom(pred, _, Polarity.Positive, _, expected) =>
                        // Collect the bound variables in a list so they can be used
                        // to by `lookup`.
                        // `(2, val)` being in the list means that attribute 3 of the tuple
                        // must have value `val`. View this as a `zipWithIndex |> filter`.
                        let filterAsList: Vector[(Int32, Boxed)] = ((Nil, 0), expected)
                            ||> Vector.foldLeft(match (acc, i) -> t -> match t {
                                case BodyTerm.Wild => (acc, i + 1)
                                case BodyTerm.Var(v) => match MutMap.get(v, getValOfVar) {
                                    case None => (acc, i + 1)
                                    case Some(val) => ((i, val) :: acc, i + 1)
                                }
                                case BodyTerm.Lit(val) => ((i, val) :: acc, i + 1)
                            }) |> fst |> List.toVector |> Vector.reverse;
                        // List of `VarSym` that become bound when the atom is
                        // satisfied.
                        let freeVars = expected |>
                            Vector.filterMap(t -> match t {
                                case BodyTerm.Var(v) => match MutMap.memberOf(v, getValOfVar) {
                                    case false => Some(v)
                                    case true => None
                                }
                                // We need to keep the order and positions for `registerVars`.
                                // We do not care about what `_` has been assigned to.
                                case BodyTerm.Wild => Some(VarSym.VarSym("_"))
                                case _ => None
                            });
                        // Get a list of all values that match the current assignment of body atoms.
                        let matches = lookup(rc, provIdb, filterAsList, Vector.length(expected), lookupStruct, pred);
                        match matches {
                            case None => false // Nothing matches the current assignment. Return false to indicate this.
                            case Some(facts) => {
                                // Go through the found facts and filter out any fact that has a depth higher than
                                // the fact we are trying to prove.
                                // For the kept facts check whether they satisfy the whole rule and if not discard them.
                                facts |>
                                    List.exists(match (bodyFact, bodyDepth, bodyRule) -> {
                                        if (bodyDepth >= depth) {
                                            // The depth of the body fact is higher than the depth of the head.
                                            // Reject this t oavoid infinite loops.
                                            false
                                        } else {
                                            // This combination could be delayed until we knew we had a satisfying assignment.
                                            Array.put(Some((combineToFact(filterAsList, bodyFact), bodyDepth, bodyRule)), index, bodyValues);
                                            registerVars(freeVars, bodyFact);
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
                        let isMatch = match Map.get(makeFakeRelSym(pred), provIdb) {
                            case None => false
                            case Some(facts) => BPlusTree.memberOf(tuple, facts)
                        };
                        if (isMatch)
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
                        if (f(getVal(v1)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard2(f, v1, v2) =>
                        if (f(getVal(v1), getVal(v2)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard3(f, v1, v2, v3) =>
                        if (f(getVal(v1), getVal(v2), getVal(v3)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard4(f, v1, v2, v3, v4) =>
                        if (f(getVal(v1), getVal(v2), getVal(v3), getVal(v4)))
                            loop(index + 1)
                        else false
                    case BodyPredicate.Guard5(f, v1, v2, v3, v4, v5) =>
                        if (f(getVal(v1), getVal(v2), getVal(v3), getVal(v4), getVal(v5)))
                            loop(index + 1)
                        else false
                    // Functional in provenance is not supported at the moment.
                    case BodyPredicate.Functional(_, _, _) => unreachable!()
                }
            }};
        // Bind all variables in the head atom to the values of `fact`.
        foreach ((i, h) <- ForEach.withIndex(headTerms)) {
            match h {
                case HeadTerm.Var(v) => MutMap.put(v, Vector.get(i, fact), getValOfVar)
                case _ => ()
            }
        };
        loop(0);
        bodyValues |> Array.filterMap(rc, identity) |> Array.toVector
    }

    ///
    /// Combines `filter` and `newValues` to a single vector.
    ///
    /// This is used to combine the search/lookup for a fact with the unknown/newly bound values.
    ///
    /// As an for `filter = [(1, Box(99)), (3, Box(42))]` and `newValues = [Box(23), Box(55), Box(77)]`
    /// Would return `[Box(23), Box(99), Box(55), Box(42), Box(77)]`
    ///
    def combineToFact(filter: Vector[(Int32, Boxed)], newValues: Vector[Boxed]): Vector[Boxed] = region rc {
        let c1 = Counter.fresh(rc);
        let c2 = Counter.fresh(rc);
        let size = Vector.length(filter) + Vector.length(newValues);
        Vector.init(i -> {
                if (Counter.get(c1) < Vector.length(filter) and i == fst(Vector.get(Counter.get(c1), filter))) {
                    snd(Vector.get(Counter.getAndIncrement(c1), filter))
                } else {
                    Vector.get(Counter.getAndIncrement(c2), newValues)
                }
        }, size)
    }
}
