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
/// Purpose of phase: For `PredSym(name, id)` we want `id` to be unique, for the whole program.
/// This phase passes over the Datalog-Ast and replaces the `id` and after everything is done
/// recreates the old `PredSym`s for the resulting model.
///
/// This is necessary as Flix uses the id-part for generating local predicates, not as unique identifiers.
///
///
/// This purpose of this module is to transform `PredSym`s defined by `(name, id)`, to `PredSym`s
/// uniquely defined by their `id`.
///
mod Fixpoint3.Phase.RenamePredSyms {
    use Fixpoint3.Ast.Datalog.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Polarity, VarSym}
    use Fixpoint3.Ast.Datalog.Datalog.{Datalog, Model, Join, Provenance}
    use Fixpoint3.Ast.Datalog.Constraint.Constraint
    use Fixpoint3.Ast.Datalog.HeadPredicate
    use Fixpoint3.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint3.Ast.Datalog.BodyPredicate.{BodyAtom, Functional, Guard1, Guard2, Guard3, Guard4, Guard5}
    use Fixpoint3.Ast.Ram.RelSym
    use Fixpoint3.Ast.Shared.{Denotation, isRelational, PredSym}
    use Fixpoint3.Ast.Shared.PredSym.PredSym
    use Fixpoint3.UniqueInts
    use Fixpoint3.Util.getOrCrash

    type alias FixInformation = Map[Int32, PredSym]

    ///
    /// Maps all `PredSym`s in `d` to have unique ids. Returns the resulting
    /// program and information which `renamePredSyms` can use to recreate the original program.
    ///
    /// This function expects `PredSym`s to be defined by their `(name, id)`, while afterwards they
    /// will be uniquely defined by their `id`.
    ///
    @Internal
    pub def assignUniqueIdentifiersToPredSyms(rc: Region[r], d: Datalog): (Datalog, FixInformation) \ r =
        let mapping = UniqueInts.empty(rc);
        let fixed = fixPredSymsInternal(d, mapping);
        (fixed, UniqueInts.reverse(mapping))

    ///
    /// Recursively applies `mapping` to the different Datalog enums to make `PredSym`s unique.
    ///
    def fixPredSymsInternal(d: Datalog, mapping: UniqueInts[PredSym, r]): Datalog \ r = match d {
        case Datalog(facts, rules) =>
            let transformer = mapForward(mapping);
            Datalog(remapConstraints(facts, transformer), remapConstraints(rules, transformer))
        case Model(facts) =>
            (Map#{}, facts)
                ||> Map.foldLeftWithKey(acc -> match RelSym.Symbol(predSym, arity, den) -> v -> {
                    let newSym = RelSym.Symbol(mapForward(mapping, predSym), arity, den);
                    Map.insert(newSym, v, acc)
                })
                |> Model
        case Provenance(rules, facts) =>
            let transformer = mapForward(mapping);
            let newRules = remapConstraints(rules, transformer);
            (Map#{}, facts)
                ||> Map.foldLeftWithKey(acc -> match RelSym.Symbol(predSym, arity, den) -> v ->
                    let newSym = RelSym.Symbol(mapForward(mapping, predSym), arity, den);
                    Map.insert(newSym, v, acc)
                )
                |> Provenance(newRules)
        case Join(d1, d2) =>
            Join(fixPredSymsInternal(d1, mapping), fixPredSymsInternal(d2, mapping))
        case _ => bug!("PredSym error")
    }

    ///
    /// Replaces `PredSym(name, id)` in `d` with `Map.get(id, fixing)`.
    ///
    @Internal
    pub def renamePredSyms(d: Datalog, fixing: FixInformation): Datalog = match d {
        case Datalog(facts, rules) =>
            let transformer = mapBack(fixing);
            Datalog(remapConstraints(facts, transformer), remapConstraints(rules, transformer))
        case Model(map) =>
            let transformer = mapBack(fixing);
            (Map#{}, map)
                ||> Map.foldLeftWithKey(newMap -> match RelSym.Symbol(p, arity, den) -> v ->
                    Map.insert(RelSym.Symbol(transformer(p), arity, den), v, newMap)
                )
                |> Model
        case Provenance(rules, model) => {
            let transformer = mapBack(fixing);
            let newRules = remapConstraints(rules, transformer);
            (Map#{}, model)
                ||> Map.foldLeftWithKey(newMap -> match RelSym.Symbol(p, arity, den) -> v ->
                    Map.insert(RelSym.Symbol(transformer(p), arity, den), v, newMap)
                )
                |> Provenance(newRules)
        }
        case Join(_, _) => bug!("Bad argument of type 'Join' in PredSym renaming")
    }

    ///
    /// Maps `pred` back to its original `PredSym` according to `reversing`.
    ///
    def mapBack(reversing: FixInformation, pred: PredSym): PredSym =
        // The `id`s are assigned 0 to n for `n` `PredSym`s. They should
        // be well within the range for `Int32`. Also no new `PredSym`s should
        // have been created. There is no recovery for either.
        let PredSym(_, id) = pred;
        getOrCrash(Map.get(getOrCrash(Int64.tryToInt32(id)), reversing))

    ///
    /// Maps `pred` back to its new `PredSym` according to `state`.
    ///
    def mapForward(state: UniqueInts[PredSym, r], pred: PredSym): PredSym \ r =
        let PredSym(name, _) = pred;
        let index = UniqueInts.createIndex(pred, state);
        PredSym(name, Int32.toInt64(index))

    ///
    /// Maps `PredSym`s of `constraints` according to `transformer`.
    ///
    def remapConstraints(constraints: Vector[Constraint], transformer: PredSym -> PredSym \ r): Vector[Constraint] \ r = {
        Vector.map(match Constraint(head, body) -> Constraint(remapHead(transformer, head), Vector.map(remapBody(transformer), body)), constraints)
    }

    ///
    /// Maps `PredSym`s of `head` according to `transformer`.
    ///
    def remapHead(transformer: PredSym -> PredSym \ r, head: HeadPredicate): HeadPredicate \ r = match head {
        case HeadAtom(pred, den, terms) => HeadAtom(transformer(pred), den, terms)
    }

    ///
    /// Maps `PredSym`s of `body` according to `transformer`.
    ///
    def remapBody(transformer: PredSym -> PredSym \ r, body: BodyPredicate): BodyPredicate \ r = match body {
        case BodyAtom(pred, den, pol, fixed, terms) => BodyAtom(transformer(pred), den, pol, fixed, terms)
        case _ => body
    }
}
