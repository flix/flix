/*
 * Copyright 2021 Benjamin Dahse
 * Copyright 2022 Jonathan Lindegaard Starup
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

mod Fixpoint.Ast.Datalog {

    use Fixpoint.PredSymsOf
    use Fixpoint.PredSymsOf.predSymsOf
    use Fixpoint.SubstitutePredSym
    use Fixpoint.SubstitutePredSym.substitute
    use Fixpoint.Shared.PredSym
    use Fixpoint.Ast.Ram.RamSym
    use Fixpoint.Ast.Denotation

    /////////////////////////////////////////////////////////////////////////////
    // Datalog                                                                 //
    /////////////////////////////////////////////////////////////////////////////

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
            case Datalog.Model(m)              => Map.keysOf(m) |> Set.toList |> List.map(predSymsOf) |> Monoid.fold
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
            case Datalog.Datalog(facts, rules) => region rc {
                let sb = StringBuilder.new(rc);
                let appendConstraint = f -> StringBuilder.appendLineWith!(ToString.toString, f, sb);
                foreach (f <- facts) appendConstraint(f);
                foreach (ru <- rules) appendConstraint(ru);
                StringBuilder.toString(sb)
            }
            case Datalog.Model(db) => region rc {
                use Fixpoint.Ast.Ram.toDenotation;
                let sb = StringBuilder.new(rc);
                Map.forEach(ramSym -> rel -> match toDenotation(ramSym) {
                    case Denotation.Relational => Map.forEach((tuple, _) -> {
                        let tupleString = tuple |> Vector.map(Debug.stringify) |> Vector.join(", ");
                        StringBuilder.appendLine!("${ramSym}(${tupleString}).", sb)
                    }, rel)
                    case Denotation.Latticenal(_) => Map.forEach((tuple, lat) -> {
                        let tupleString = tuple |> Vector.map(Debug.stringify) |> Vector.join(", ");
                        StringBuilder.appendLine!("${ramSym}(${tupleString}; %{lat}).", sb)
                    }, rel)
                }, db);
                StringBuilder.toString(sb)
            }
            case Datalog.Join(d1, d2) => region rc {
                let sb = StringBuilder.new(rc);
                StringBuilder.appendLine!(ToString.toString(d1), sb);
                StringBuilder.appendLine!(ToString.toString(d2), sb);
                StringBuilder.toString(sb)
            }
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // Constraint                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum Constraint[v] {
        case Constraint(HeadPredicate[v], Vector[BodyPredicate[v]])
    }

    instance PredSymsOf[Constraint[v]] {
        pub def predSymsOf(x: Constraint[v]): Set[PredSym] = match x {
            case Constraint.Constraint(head, body) =>
                let headSyms = predSymsOf(head);
                let bodySyms = Vector.map(predSymsOf, body);
                headSyms ++ Monoid.fold(bodySyms)
        }
    }

    instance SubstitutePredSym[Constraint[v]] {
        pub def substitute(x: Constraint[v], s: Map[PredSym, PredSym]): Constraint[v] = match x {
            case Constraint.Constraint(head, body) =>
                let newHead = substitute(head, s);
                let newBody = Vector.map(p -> substitute(p, s), body);
                Constraint.Constraint(newHead, newBody)
        }
    }

    instance ToString[Constraint[v]] {
        pub def toString(c: Constraint[v]): String =
            match c {
                case Constraint.Constraint(head, body) =>
                    if (Vector.length(body) == 0)
                        "${head}."
                    else
                        "${head} :- ${body |> Vector.join(", ")}."
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // HeadPredicate                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum HeadPredicate[v] {
        case HeadAtom(PredSym, Denotation[v], Vector[HeadTerm[v]])
    }

    instance PredSymsOf[HeadPredicate[v]] {
        pub def predSymsOf(x: HeadPredicate[v]): Set[PredSym] = match x {
            case HeadPredicate.HeadAtom(predSym, _, _ ) => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[HeadPredicate[v]] {
        pub def substitute(x: HeadPredicate[v], s: Map[PredSym, PredSym]): HeadPredicate[v] = match x {
            case HeadPredicate.HeadAtom(predSym, den, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                HeadPredicate.HeadAtom(newSym, den, terms)
        }
    }

    instance ToString[HeadPredicate[v]] {
        pub def toString(head: HeadPredicate[v]): String =
            match head {
                case HeadPredicate.HeadAtom(predSym, Denotation.Relational, terms) => "${predSym}(${terms |> Vector.join(", ")})"
                case HeadPredicate.HeadAtom(predSym, Denotation.Latticenal(_), terms) =>
                    let (keyTerms, latticeTerms) = Vector.splitAt(Vector.length(terms)-1, terms);
                    match Vector.head(latticeTerms) {
                        case None    => "${predSym}(${keyTerms |> Vector.join(", ")})"
                        case Some(l) => "${predSym}(${keyTerms |> Vector.join(", ")}; ${l})"
                    }
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // BodyPredicate                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum BodyPredicate[v] {
        case BodyAtom(PredSym, Denotation[v], Polarity, Fixity, Vector[BodyTerm[v]])
        case Functional(Vector[VarSym], Vector[v] -> Vector[Vector[v]], Vector[VarSym])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, VarSym)
        case Guard2(v -> v -> Bool, VarSym, VarSym)
        case Guard3(v -> v -> v -> Bool, VarSym, VarSym, VarSym)
        case Guard4(v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym)
        case Guard5(v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym)
    }

    instance PredSymsOf[BodyPredicate[v]] {
        pub def predSymsOf(x: BodyPredicate[v]): Set[PredSym] = match x {
            case BodyPredicate.BodyAtom(predSym, _, _, _, _)    => Set.singleton(predSym)
            case _                                              => Set.empty()
        }
    }

    instance SubstitutePredSym[BodyPredicate[v]] {
        pub def substitute(x: BodyPredicate[v], s: Map[PredSym, PredSym]): BodyPredicate[v] = match x {
            case BodyPredicate.BodyAtom(predSym, den, polarity, fixity, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                BodyPredicate.BodyAtom(newSym, den, polarity, fixity, terms)
            case _                             => x
        }
    }

    instance ToString[BodyPredicate[v]] {
        pub def toString(body: BodyPredicate[v]): String =
            def polarityPrefix(p) = match p {
                case Polarity.Negative => "not "
                case Polarity.Positive => ""
            };
            def fixityPrefix(f) = match f {
                case Fixity.Fixed => "fix "
                case Fixity.Loose => ""
            };
            match body {
                case BodyPredicate.BodyAtom(predSym, Denotation.Relational, p, f, terms) =>
                    "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${terms |> Vector.join(", ")})"
                case BodyPredicate.BodyAtom(predSym, Denotation.Latticenal(_), p, f, terms) =>
                    let n = Vector.length(terms)-1;
                    let (keyTerms, latticeTerms) = (Vector.take(n, terms), Vector.drop(n, terms));
                    match Vector.head(latticeTerms) {
                        case None    => "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}()"
                        case Some(l) => "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${keyTerms |> Vector.join(", ")}; ${l})"
                    }
                case BodyPredicate.Functional(boundVars, _, freeVars) => "<loop>(${boundVars}, ${freeVars})"
                case BodyPredicate.Guard0(_) => "<clo>()"
                case BodyPredicate.Guard1(_, v) => "<clo>(${v})"
                case BodyPredicate.Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case BodyPredicate.Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case BodyPredicate.Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BodyPredicate.Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // HeadTerm                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum HeadTerm[v] {
        case Var(VarSym)
        case Lit(v)
        case App0(Unit -> v)
        case App1(v -> v, VarSym)
        case App2(v -> v -> v, VarSym, VarSym)
        case App3(v -> v -> v -> v, VarSym, VarSym, VarSym)
        case App4(v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym)
        case App5(v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym)
    }

    instance ToString[HeadTerm[v]] {
        pub def toString(term: HeadTerm[v]): String = match term {
            case HeadTerm.Var(varSym) => "${varSym}"
            case HeadTerm.Lit(v) => "%{v}"
            case HeadTerm.App0(_) => "<clo>()"
            case HeadTerm.App1(_, v) => "<clo>(${v})"
            case HeadTerm.App2(_, v1, v2) => "<clo>(${v1}, ${v2})"
            case HeadTerm.App3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
            case HeadTerm.App4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case HeadTerm.App5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // BodyTerm                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum BodyTerm[v] {
        case Wild
        case Var(VarSym)
        case Lit(v)
    }

    instance ToString[BodyTerm[v]] {
        pub def toString(term: BodyTerm[v]): String = match term {
            case BodyTerm.Wild => "_"
            case BodyTerm.Var(varSym) => ToString.toString(varSym)
            case BodyTerm.Lit(v) => Debug.stringify(v)
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // VarSym                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum VarSym with Eq, Order, ToString {
        case VarSym(String)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Fixity                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum Fixity {
        case Loose,
        case Fixed
    }

    /////////////////////////////////////////////////////////////////////////////
    // Polarity                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum Polarity {
        case Positive,
        case Negative
    }

}
