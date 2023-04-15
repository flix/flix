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

mod Fixpoint.Ast {

    use Fixpoint.PredSymsOf
    use Fixpoint.PredSymsOf.predSymsOf
    use Fixpoint.SubstitutePredSym
    use Fixpoint.SubstitutePredSym.substitute
    use Fixpoint.Shared.PredSym
    use Fixpoint.Ram.RamSym

    @Internal
    pub enum Datalog[v] {
        case Datalog(Vector[Constraint[v]], Vector[Constraint[v]])
        case Model(Map[RamSym[v], Map[Vector[v], v]])
        case Join(Datalog[v], Datalog[v])
    }

    instance PredSymsOf[Datalog[v]] {
        pub def predSymsOf(x: Datalog[v]): Set[PredSym] = match x {
            case Datalog.Datalog(facts, rules) =>
                let factSyms = facts |> Vector.map(predSymsOf) |> Monoid.fold;
                let ruleSyms = rules |> Vector.map(predSymsOf) |> Monoid.fold;
                factSyms ++ ruleSyms
            case Datalog.Model(m)              => Map.keysOf(m) |> Set.toVector |> Vector.map(predSymsOf) |> Monoid.fold
            case Datalog.Join(v1, v2)          => predSymsOf(v1) ++ predSymsOf(v2)
        }
    }

    instance SubstitutePredSym[Datalog[v]] {
        pub def substitute(x: Datalog[v], s: Map[PredSym, PredSym]): Datalog[v] = match x {
            case Datalog.Datalog(facts, rules) =>
                let newFacts = Vector.map(c -> substitute(c, s), facts);
                let newRules = Vector.map(c -> substitute(c, s), rules);
                Datalog.Datalog(newFacts, newRules)
            case Datalog.Model(m) =>
                def f(macc, sym, v) = {
                    let newSym = substitute(sym, s);
                    Map.insert(newSym, v, macc)
                };
                Datalog.Model(Map.foldLeftWithKey(f, Map.empty(), m))
            case Datalog.Join(v1, v2) => Datalog.Join(substitute(v1, s), substitute(v2, s))
        }
    }

    instance ToString[Datalog[v]] {
        pub def toString(cs: Datalog[v]): String = match cs {
            case Datalog.Datalog(facts, rules) => region r {
                let sb = StringBuilder.new(r);
                let appendConstraint = f -> StringBuilder.appendLineWith!(ToString.toString, f, sb);
                foreach (f <- facts) appendConstraint(f);
                foreach (ru <- rules) appendConstraint(ru);
                StringBuilder.toString(sb)
            }
            case Datalog.Model(db) => region r {
                use Fixpoint.Ram.toDenotation;
                let sb = StringBuilder.new(r);
                Map.forEach(ramSym -> rel -> match toDenotation(ramSym) {
                    case Denotation.Relational => Map.forEach((tuple, _) -> {
                        let tupleString = Vector.toList(tuple) |> List.map(Debug.stringify) |> List.join(", ");
                        StringBuilder.appendLine!("${ramSym}(${tupleString}).", sb)
                    }, rel)
                    case Denotation.Latticenal(_) => Map.forEach((tuple, lat) -> {
                        let tupleString = Vector.toList(tuple) |> List.map(Debug.stringify) |> List.join(", ");
                        StringBuilder.appendLine!("${ramSym}(${tupleString}; %{lat}).", sb)
                    }, rel)
                }, db);
                StringBuilder.toString(sb)
            }
            case Datalog.Join(d1, d2) => region r {
                let sb = StringBuilder.new(r);
                StringBuilder.appendLine!(ToString.toString(d1), sb);
                StringBuilder.appendLine!(ToString.toString(d2), sb);
                StringBuilder.toString(sb)
            }
        }
    }
}
