/*
 * Copyright 2021 Benjamin Dahse
 * Copyright 2022 Jonathan Lindegaard Starup
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

mod Fixpoint.Ast.Datalog {
    use Fixpoint.Boxed;

    use Fixpoint.PredSymsOf
    use Fixpoint.PredSymsOf.predSymsOf
    use Fixpoint.SubstitutePredSym
    use Fixpoint.SubstitutePredSym.substitute
    use Fixpoint.Ast.Ram.RelSym
    use Fixpoint.Ast.Shared.Denotation
    use Fixpoint.Ast.Shared.PredSym

    /////////////////////////////////////////////////////////////////////////////
    // Datalog                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum Datalog {
        case Datalog(Vector[Constraint], Vector[Constraint])
        case Model(Map[RelSym, Map[Vector[Boxed], Boxed]])
        case Join(Datalog, Datalog)
    }

    instance PredSymsOf[Datalog] {
        pub def predSymsOf(x: Datalog): Set[PredSym] = match x {
            case Datalog.Datalog(facts, rules) =>
                let factSyms = facts |> Vector.map(predSymsOf) |> Monoid.fold;
                let ruleSyms = rules |> Vector.map(predSymsOf) |> Monoid.fold;
                factSyms ++ ruleSyms
            case Datalog.Model(m)              => Map.keysOf(m) |> Set.toList |> List.map(predSymsOf) |> Monoid.fold
            case Datalog.Join(v1, v2)          => predSymsOf(v1) ++ predSymsOf(v2)
        }
    }

    instance SubstitutePredSym[Datalog] {
        pub def substitute(x: Datalog, s: Map[PredSym, PredSym]): Datalog = match x {
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

    instance ToString[Datalog] {
        pub def toString(cs: Datalog): String = match cs {
            case Datalog.Datalog(facts, rules) => region rc {
                Iterator.append(Vector.iterator(rc, facts), Vector.iterator(rc, rules))
                    |> Iterator.join(String.lineSeparator())
            }
            case Datalog.Model(db) => region rc {
                use Fixpoint.Ast.Ram.toDenotation;
                Map.iterator(rc, db)
                    |> Iterator.flatMap(match (ramSym, rel) -> {
                        Map.iterator(rc, rel) |> Iterator.map(match (tuple, lat) -> {
                            let tupleString = tuple |> Vector.map(Debug.stringify) |> Vector.join(", ");
                            match toDenotation(ramSym) {
                                case Denotation.Relational => "${ramSym}(${tupleString})."
                                case Denotation.Latticenal(_, _, _, _) => "${ramSym}(${tupleString}; %{lat})."
                            }
                        })
                    })
                    |> Iterator.join(String.lineSeparator())
                    + String.lineSeparator()
            }
            case Datalog.Join(d1, d2) => {
                let lineSep = String.lineSeparator();
                "${d1}${lineSep}${d2}${lineSep}"
            }
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // Constraint                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum Constraint {
        case Constraint(HeadPredicate, Vector[BodyPredicate])
    }

    instance PredSymsOf[Constraint] {
        pub def predSymsOf(x: Constraint): Set[PredSym] = match x {
            case Constraint.Constraint(head, body) =>
                let headSyms = predSymsOf(head);
                let bodySyms = Vector.map(predSymsOf, body);
                headSyms ++ Monoid.fold(bodySyms)
        }
    }

    instance SubstitutePredSym[Constraint] {
        pub def substitute(x: Constraint, s: Map[PredSym, PredSym]): Constraint = match x {
            case Constraint.Constraint(head, body) =>
                let newHead = substitute(head, s);
                let newBody = Vector.map(p -> substitute(p, s), body);
                Constraint.Constraint(newHead, newBody)
        }
    }

    instance ToString[Constraint] {
        pub def toString(c: Constraint): String =
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
    pub enum HeadPredicate {
        case HeadAtom(PredSym, Denotation, Vector[HeadTerm])
    }

    instance PredSymsOf[HeadPredicate] {
        pub def predSymsOf(x: HeadPredicate): Set[PredSym] = match x {
            case HeadPredicate.HeadAtom(predSym, _, _ ) => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[HeadPredicate] {
        pub def substitute(x: HeadPredicate, s: Map[PredSym, PredSym]): HeadPredicate = match x {
            case HeadPredicate.HeadAtom(predSym, den, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                HeadPredicate.HeadAtom(newSym, den, terms)
        }
    }

    instance ToString[HeadPredicate] {
        pub def toString(head: HeadPredicate): String =
            match head {
                case HeadPredicate.HeadAtom(predSym, Denotation.Relational, terms) => "${predSym}(${terms |> Vector.join(", ")})"
                case HeadPredicate.HeadAtom(predSym, Denotation.Latticenal(_, _, _, _), terms) =>
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
    pub enum BodyPredicate {
        case BodyAtom(PredSym, Denotation, Polarity, Fixity, Vector[BodyTerm])
        case Functional(Vector[VarSym], Vector[Boxed] -> Vector[Vector[Boxed]], Vector[VarSym])
        case Guard0(Unit -> Bool)
        case Guard1(Boxed -> Bool, VarSym)
        case Guard2(Boxed -> Boxed -> Bool, VarSym, VarSym)
        case Guard3(Boxed -> Boxed -> Boxed -> Bool, VarSym, VarSym, VarSym)
        case Guard4(Boxed -> Boxed -> Boxed -> Boxed -> Bool, VarSym, VarSym, VarSym, VarSym)
        case Guard5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym)
    }

    instance PredSymsOf[BodyPredicate] {
        pub def predSymsOf(x: BodyPredicate): Set[PredSym] = match x {
            case BodyPredicate.BodyAtom(predSym, _, _, _, _)    => Set.singleton(predSym)
            case _                                              => Set.empty()
        }
    }

    instance SubstitutePredSym[BodyPredicate] {
        pub def substitute(x: BodyPredicate, s: Map[PredSym, PredSym]): BodyPredicate = match x {
            case BodyPredicate.BodyAtom(predSym, den, polarity, fixity, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                BodyPredicate.BodyAtom(newSym, den, polarity, fixity, terms)
            case _                             => x
        }
    }

    instance ToString[BodyPredicate] {
        pub def toString(body: BodyPredicate): String =
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
                case BodyPredicate.BodyAtom(predSym, Denotation.Latticenal(_, _, _, _), p, f, terms) =>
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
    pub enum HeadTerm {
        case Var(VarSym)
        case Lit(Boxed)
        case App0(Unit -> Boxed)
        case App1(Boxed -> Boxed, VarSym)
        case App2(Boxed -> Boxed -> Boxed, VarSym, VarSym)
        case App3(Boxed -> Boxed -> Boxed -> Boxed, VarSym, VarSym, VarSym)
        case App4(Boxed -> Boxed -> Boxed -> Boxed -> Boxed, VarSym, VarSym, VarSym, VarSym)
        case App5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed, VarSym, VarSym, VarSym, VarSym, VarSym)
    }

    instance ToString[HeadTerm] {
        pub def toString(term: HeadTerm): String = match term {
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
    pub enum BodyTerm {
        case Wild
        case Var(VarSym)
        case Lit(Boxed)
    }

    instance ToString[BodyTerm] {
        pub def toString(term: BodyTerm): String = match term {
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
