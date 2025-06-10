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

//
/// For the rest of the compiler we expect a PredSym, `PredSym(name, id)`, to have unique `id`.
/// This phase passes over the Datalog-Ast and replaces the id and after everything is done
/// recreates the old `PredSym`s for the resulting model.
/// 
/// It also contains method for recreating the original PredSyms such that the model given to Flix
/// is correct.
//
mod Fixpoint3.Phase.RenamePredSyms {
    use Fixpoint.Ast.Datalog.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Polarity, VarSym}
    use Fixpoint.Ast.Datalog.Datalog.{Datalog, Model, Join}
    use Fixpoint.Ast.Datalog.Constraint.Constraint
    use Fixpoint.Ast.Datalog.HeadPredicate
    use Fixpoint.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint.Ast.Datalog.BodyPredicate.{BodyAtom, Functional, Guard1, Guard2, Guard3, Guard4, Guard5}
    use Fixpoint3.Ast.Ram.RelSym
    use Fixpoint.Ast.Shared.{Denotation, isRelational, PredSym}
    use Fixpoint.Ast.Shared.PredSym.PredSym
    use Fixpoint3.UniqueInts
    use Fixpoint3.Util.getOrCrash

    type alias FixInformation = Map[Int32, PredSym]

    ///
    /// Maps all `PredSym`s in `d` to have unique ids. Returns the resulting
    /// program and information which `renamePredSymToFlixStyle` can use to recreate the original program.
    ///
    @Internal
    pub def renamePredSyms(rc: Region[r], d: Datalog): (Datalog, FixInformation) \ r =
        let mapping = UniqueInts.empty(rc);
        let fixed = renamePredSymsInternal(d, mapping);
        (fixed, UniqueInts.reverse(mapping))

    ///
    /// Recursively applies `mapping` to the different Datalog enums to make `PredSym`s unique.
    ///
    def renamePredSymsInternal(d: Datalog, mapping: UniqueInts[PredSym, r]): Datalog \ r = match d {
        case Datalog(facts, rules) =>
            let transformer = mapForward(mapping);
            Datalog(remapConstraints(facts, transformer), remapConstraints(rules, transformer))
        case Model(facts) =>
            Model(Map.foldLeftWithKey(acc -> k -> v -> match k {
                case RelSym.Symbol(predSym, arity, den) =>
                    let newSym = RelSym.Symbol(mapForward(mapping, predSym), arity, den);
                    Map.insert(newSym, v, acc)
            }, Map#{}, facts))
        case Join(d1, d2) =>
            Join(renamePredSymsInternal(d1, mapping), renamePredSymsInternal(d2, mapping))
        case _ => bug!("PredSym error")
    }

    ///
    /// Applies `fixing` to the `d` to restore the `PredSym`s to the original values.
    ///
    @Internal
    pub def renamePredSymToFlixStyle(d: Datalog, fixing: FixInformation): Datalog = match d {
        case Datalog(facts, rules) => 
            let transformer = mapBack(fixing);
            Datalog(remapConstraints(facts, transformer), remapConstraints(rules, transformer))
        case Model(map) => 
            let transformer = mapBack(fixing);
            Map.foldLeftWithKey(newMap -> k -> v -> match k {
                case RelSym.Symbol(p, arity, den) => Map.insert(RelSym.Symbol(transformer(p), arity, den), v, newMap)
            }, Map#{}, map) |> 
            Model
        case _ => bug!("Bad argument of type 'Join' in PredSym renaming")
    }
    
    ///
    /// Maps `pred` back to its original `PredSym` according to `reversing`.
    ///
    def mapBack(reversing: FixInformation, pred: PredSym): PredSym = 
        let PredSym(_, id) = pred;
        getOrCrash(Map.get(getOrCrash(Int64.tryToInt32(id)), reversing))

    ///
    /// Maps `pred` back to its new `PredSym` according to `state`.
    ///
    def mapForward(state: UniqueInts[PredSym, r], pred: PredSym): PredSym \ r = 
        let PredSym(name, _) = pred;
        let index = UniqueInts.getIndex(pred, state);
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