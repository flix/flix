/*
 * Copyright 2021 Benjamin Dahse
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
use Fixpoint.PredSymsOf;
use Fixpoint/PredSymsOf.predSymsOf;
use Fixpoint.SubstitutePredSym;
use Fixpoint/SubstitutePredSym.substitute;
use Fixpoint.Tuple;
use Fixpoint.Tuple.Tuple;
use Fixpoint/Shared.PredSym;
use Fixpoint/Ram.RamSym;
use ToString.toString;

namespace Fixpoint/Ast {
    pub enum Datalog[v] {
        case Datalog(Array[Constraint[v], Static], Array[Constraint[v], Static])
        case Model(Map[RamSym[v], Map[Tuple[v], v]])
        case Join(Datalog[v], Datalog[v])
    }

    instance PredSymsOf[Datalog[v]] {
        pub def predSymsOf(x: Datalog[v]): Set[PredSym] = match x {
            case Datalog(facts, rules) =>
                let factSyms = facts |> Array.map(predSymsOf) |> Array.toList |> Monoid.fold;
                let ruleSyms = rules |> Array.map(predSymsOf) |> Array.toList |> Monoid.fold;
                factSyms ++ ruleSyms
            case Model(m)              => Map.keysOf(m) |> Set.toList |> List.map(predSymsOf) |> Monoid.fold
            case Join(v1, v2)          => predSymsOf(v1) ++ predSymsOf(v2)
        } as & Pure
    }

    instance SubstitutePredSym[Datalog[v]] {
        pub def substitute(x: Datalog[v], s: Map[PredSym, PredSym]): Datalog[v] = match x {
            case Datalog(facts, rules) =>
                let newFacts = Array.map(c -> substitute(c, s), facts);
                let newRules = Array.map(c -> substitute(c, s), rules);
                Datalog(newFacts, newRules)
            case Model(m) =>
                def f(macc, sym, v) = {
                    let newSym = substitute(sym, s);
                    Map.insert(newSym, v, macc)
                };
                Model(Map.foldLeftWithKey(f, Map.empty(), m))
            case Join(v1, v2) => Join(substitute(v1, s), substitute(v2, s))
        } as & Pure
    }

    instance ToString[Datalog[v]] with ToString[v] {
        pub def toString(cs: Datalog[v]): String = match cs {
            case Datalog(facts, rules) =>
                let sb = new StringBuilder(Static);
                StringBuilder.appendLinesWith!(ToString.toString, facts, sb);
                StringBuilder.appendLinesWith!(ToString.toString, rules, sb);
                StringBuilder.toString(sb)
            case Model(db) =>
                use Fixpoint/Ram.toDenotation;
                use Fixpoint.commaSeparate;
                let sb = new StringBuilder(Static);
                Map.foreach(ramSym -> rel -> match toDenotation(ramSym) {
                    case Relational => Map.foreach(match Tuple(tuple) -> _ -> {
                        StringBuilder.appendLine!("${ramSym}(${commaSeparate(tuple)}).", sb)
                    }, rel)
                    case Latticenal(_) => Map.foreach(match Tuple(tuple) -> lat -> {
                        StringBuilder.appendLine!("${ramSym}(${commaSeparate(tuple)}; ${lat}).", sb)
                    }, rel)
                }, db);
                StringBuilder.toString(sb)
            case Join(d1, d2) =>
                let sb = new StringBuilder(Static);
                StringBuilder.appendLine!(ToString.toString(d1), sb);
                StringBuilder.appendLine!(ToString.toString(d2), sb);
                StringBuilder.toString(sb)
        } as & Pure
    }
}
