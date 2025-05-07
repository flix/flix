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
Purpose of file:
Apparently the rest of the Flix compiler does not use the int part of PredSym's at all
Therefore remap all PredSym's such the int part can be used as an identifier, instead of the string.
*/

mod Fixpoint.Phase.FixPredSyms {
    use Fixpoint.UniqueInts

    use Fixpoint.Ast.Ram.{RelSym}
    use Fixpoint.Ast.Datalog.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Polarity, VarSym}
    use Fixpoint.Ast.Datalog.Datalog.{Datalog, Model, Join}
    use Fixpoint.Ast.Datalog.Constraint.Constraint
    use Fixpoint.Ast.Datalog.HeadPredicate
    use Fixpoint.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint.Ast.Datalog.BodyPredicate.{BodyAtom}
    use Fixpoint.Ast.Datalog.BodyPredicate.{Functional}
    use Fixpoint.Ast.Datalog.BodyPredicate.{Guard0, Guard1, Guard2, Guard3, Guard4, Guard5}
    use Fixpoint.Ast.Shared.Denotation
    use Fixpoint.Ast.Shared.isRelational
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Ast.Shared.PredSym.PredSym
    use Fixpoint.Predicates.{PredSymMapInternal}
    use Fixpoint.Predicates.PredType.{Full, Delta, New}
    use Fixpoint.UniqueInts.{IndexState, ReverseState}
    use Fixpoint.Helpers.unwrap


    pub type alias FixInformation = UniqueInts.ReverseState[PredSym]

    @Internal
    pub def fixPredSyms(d: Datalog): (Datalog, FixInformation) = region rc {
        let mapping = UniqueInts.empty(rc);
        let fixed = fixPredSymsInternal(d, mapping);
        (fixed, UniqueInts.reverse(mapping))
    }

    @Internal
    pub def fixPredSymsInternal(d: Datalog, mapping: IndexState[PredSym, r]): Datalog \ r = match d {
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
            Join(fixPredSymsInternal(d1, mapping), fixPredSymsInternal(d2, mapping))
        case _ => bug!("PredSym error")
    }


    @Internal
    pub def unfixPredSym(d: Datalog, fixing: FixInformation): Datalog = match d {
        case Datalog(facts, rules) => 
            let transformer = mapBack(fixing);
            Datalog(remapConstraints(facts, transformer), remapConstraints(rules, transformer))
        case Model(map) => 
            let transformer = mapBack(fixing);
            Map.foldLeftWithKey(newMap -> k -> v -> match k {
                case RelSym.Symbol(p, arity, den) => Map.insert(RelSym.Symbol(transformer(p), arity, den), v, newMap)
            }, Map#{}, map)
            |> Model
        case _ => bug!("Bad argument of type 'Join' in PredSym renaming")
    }
    
    @Internal
    def mapBack(reversing: ReverseState[PredSym], pred: PredSym): PredSym = 
        let PredSym(_, id) = pred;
        match Map.get(unwrap(Int64.tryToInt32(id)), reversing) {
            case Some(v) => v
            case _ => bug!("Incomplete mapping given to when 'unfixing predicates'")
        }
    

    @Internal
    def mapForward(state: IndexState[PredSym, r], pred: PredSym): PredSym \ r = 
        let PredSym(name, _) = pred;
        let index = UniqueInts.getIndex(pred, state);
        PredSym(name, Int32.toInt64(index))

    @Internal
    def remapConstraints(constraints: Vector[Constraint], transformer: PredSym -> PredSym \ r): Vector[Constraint] \ r = {
        Vector.map(match Constraint(head, body) -> Constraint(remapHead(head, transformer), Vector.map(x -> remapBody(x, transformer), body)), constraints)
    }

    @Internal
    def remapHead(head: HeadPredicate, transformer: PredSym -> PredSym \ r): HeadPredicate \ r = match head {
        case HeadAtom(pred, den, terms) => HeadAtom(transformer(pred), den, terms)
    }
    
    @Internal
    def remapBody(body: BodyPredicate, transformer: PredSym -> PredSym \ r): BodyPredicate \ r = match body {
        case BodyAtom(pred, den, pol, fixed, terms) => BodyAtom(transformer(pred), den, pol, fixed, terms)
        case _ => body
    }
}