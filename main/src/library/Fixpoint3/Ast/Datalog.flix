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

pub mod Fixpoint3.Ast.Datalog {
    use Fixpoint3.Boxed;

    use Fixpoint3.PredSymsOf
    use Fixpoint3.PredSymsOf.predSymsOf
    use Fixpoint3.SubstitutePredSym
    use Fixpoint3.SubstitutePredSym.substitute
    use Fixpoint3.Ast.Ram.RelSym
    use Fixpoint3.Ast.Shared.{BoxedDenotation => Denotation}
    use Fixpoint3.Ast.Shared.Denotation.{Latticenal, Relational}
    use Fixpoint3.Ast.Shared.PredSym

    /////////////////////////////////////////////////////////////////////////////
    // Datalog                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `Datalog(facts, rules)` is a Datalog program where `facts` and `rules` are the
    /// facts and rules of the program, respectively.
    ///
    /// `Model(factsMap)` is a model mapping `RelSym` the their `IDB`/facts.
    ///
    /// `Join(p1, p2)` is the combination of Datalog programs `p1` and `p2`.
    ///
    /// `Provenance(rules, model)` is a Datalog model where `model` is a map from
    /// computed facts to annotations of the form `(depth, ruleUsed)` and `rules`
    /// are the rules used during the computation of `model`.
    ///
    pub enum Datalog {
        case Datalog(Vector[Constraint], Vector[Constraint])
        case Model(Map[RelSym, BPlusTree[Vector[Boxed], Boxed, Static]])
        case Join(Datalog, Datalog)
        case Provenance(Vector[Constraint], Map[RelSym, BPlusTree[Vector[Boxed], (Int64, Int32), Static]])
    }

    instance PredSymsOf[Datalog] {
        pub def predSymsOf(x: Datalog): Set[PredSym] = match x {
            case Datalog.Datalog(facts, rules) =>
                let factSyms = facts |> Vector.map(predSymsOf) |> Monoid.fold;
                let ruleSyms = rules |> Vector.map(predSymsOf) |> Monoid.fold;
                factSyms ++ ruleSyms
            case Datalog.Model(m)                 => Map.keysOf(m) |> Set.toList |> List.map(predSymsOf) |> Monoid.fold
            case Datalog.Join(v1, v2)             => predSymsOf(v1) ++ predSymsOf(v2)
            case Datalog.Provenance(rules, model) =>
                let ruleSyms = rules |> Vector.map(predSymsOf) |> Monoid.fold;
                let modelSyms = Map.keysOf(model) |> Set.map(predSymsOf) |> Monoid.fold;
                ruleSyms ++ modelSyms
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
            case Datalog.Join(v1, v2)             => Datalog.Join(substitute(v1, s), substitute(v2, s))
            case Datalog.Provenance(rules, model) =>
                let newRules = Vector.map(c -> substitute(c, s), rules);
                let newModel = (Map#{}, model) ||>
                    Map.foldLeftWithKey(acc -> pred -> facts ->
                        Map.insert(substitute(pred, s), facts, acc)
                    );
                Datalog.Provenance(newRules, newModel)
        }
    }

    instance ToString[Datalog] {
        pub def toString(cs: Datalog): String = match cs {
            case Datalog.Datalog(facts, rules) => region rc {
                if (not Fixpoint3.Options.enableDebugPrintFacts()) {
                    "Printing facts disabled ${String.lineSeparator()}" +
                    Vector.iterator(rc, rules) |> Iterator.join(String.lineSeparator())
                } else {
                    Iterator.append(Vector.iterator(rc, facts), Vector.iterator(rc, rules))
                        |> Iterator.join(String.lineSeparator())
                }
            }
            case Datalog.Model(db) => region rc {
                use Fixpoint3.Ast.Ram.toDenotation;
                if (not Fixpoint3.Options.enableDebugPrintFacts()) {
                    "Printing facts disabled."
                } else {
                   unsafe IO {
                        let builder = StringBuilder.empty(rc);
                        foreach ((relSym, innerMap) <- db) {
                            foreach ((tuple, lat) <- innerMap) {
                                let tupleString = tuple |> Vector.map(Debug.stringify) |> Vector.join(", ");
                                match toDenotation(relSym) {
                                    case Relational             => StringBuilder.append("${relSym}(${tupleString}).${String.lineSeparator()}", builder)
                                    case Latticenal(_, _, _, _) => StringBuilder.append("${relSym}(${tupleString}; ${Debug.stringify(lat)}).${String.lineSeparator()}", builder)
                                }
                            }
                        };
                        StringBuilder.toString(builder)
                    }
                }
            }
            case Datalog.Join(d1, d2) => {
                let lineSep = String.lineSeparator();
                "${d1}${lineSep}${d2}${lineSep}"
            }
            case Datalog.Provenance(_, db) => region rc {
                unsafe IO {
                    let indent = String.repeat(4, " ");
                    Map.joinWith(predSym -> facts -> {
                        let builder = StringBuilder.empty(rc);
                        foreach ((fact, annotation) <- facts) {
                            StringBuilder.append("${indent}${fact} @ ${annotation},${String.lineSeparator()}${indent}", builder)
                        };
                        let textOverflow = String.length("$,${String.lineSeparator()}${indent}");
                        let rawFactString = StringBuilder.toString(builder);
                        let factsString = String.sliceLeft(end = String.length(rawFactString)- textOverflow, rawFactString);
                        "${predSym}:${String.lineSeparator()}${factsString}"
                    }, "${String.lineSeparator()}", db)
                }
            }
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // Constraint                                                              //
    /////////////////////////////////////////////////////////////////////////////

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
                case HeadPredicate.HeadAtom(predSym, Relational, terms)             => "${predSym}(${terms |> Vector.join(", ")})"
                case HeadPredicate.HeadAtom(predSym, Latticenal(_, _, _, _), terms) =>
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
            case BodyPredicate.BodyAtom(predSym, _, _, _, _) => Set.singleton(predSym)
            case _                                           => Set.empty()
        }
    }

    instance SubstitutePredSym[BodyPredicate] {
        pub def substitute(x: BodyPredicate, s: Map[PredSym, PredSym]): BodyPredicate = match x {
            case BodyPredicate.BodyAtom(predSym, den, polarity, fixity, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                BodyPredicate.BodyAtom(newSym, den, polarity, fixity, terms)
            case _ => x
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
                case BodyPredicate.BodyAtom(predSym, Relational, p, f, terms) =>
                    "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${terms |> Vector.join(", ")})"
                case BodyPredicate.BodyAtom(predSym, Latticenal(_, _, _, _), p, f, terms) =>
                    let n = Vector.length(terms)-1;
                    let (keyTerms, latticeTerms) = (Vector.take(n, terms), Vector.drop(n, terms));
                    match Vector.head(latticeTerms) {
                        case None    => "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}()"
                        case Some(l) => "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${keyTerms |> Vector.join(", ")}; ${l})"
                    }
                case BodyPredicate.Functional(boundVars, _, freeVars) => "<loop>(${boundVars}, ${freeVars})"
                case BodyPredicate.Guard0(_)                          => "<clo>()"
                case BodyPredicate.Guard1(_, v)                       => "<clo>(${v})"
                case BodyPredicate.Guard2(_, v1, v2)                  => "<clo>(${v1}, ${v2})"
                case BodyPredicate.Guard3(_, v1, v2, v3)              => "<clo>(${v1}, ${v2}, ${v3})"
                case BodyPredicate.Guard4(_, v1, v2, v3, v4)          => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BodyPredicate.Guard5(_, v1, v2, v3, v4, v5)      => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // HeadTerm                                                                //
    /////////////////////////////////////////////////////////////////////////////

    pub enum HeadTerm {
        case Var(VarSym)
        case Lit(Boxed)
        case App1(Boxed -> Boxed, VarSym)
        case App2(Boxed -> Boxed -> Boxed, VarSym, VarSym)
        case App3(Boxed -> Boxed -> Boxed -> Boxed, VarSym, VarSym, VarSym)
        case App4(Boxed -> Boxed -> Boxed -> Boxed -> Boxed, VarSym, VarSym, VarSym, VarSym)
        case App5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed, VarSym, VarSym, VarSym, VarSym, VarSym)
    }

    instance ToString[HeadTerm] {
        pub def toString(term: HeadTerm): String = match term {
            case HeadTerm.Var(varSym)                 => "${varSym}"
            case HeadTerm.Lit(v)                      => Debug.stringify(v)
            case HeadTerm.App1(_, v)                  => "<clo>(${v})"
            case HeadTerm.App2(_, v1, v2)             => "<clo>(${v1}, ${v2})"
            case HeadTerm.App3(_, v1, v2, v3)         => "<clo>(${v1}, ${v2}, ${v3})"
            case HeadTerm.App4(_, v1, v2, v3, v4)     => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case HeadTerm.App5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // BodyTerm                                                                //
    /////////////////////////////////////////////////////////////////////////////

    pub enum BodyTerm {
        case Wild
        case Var(VarSym)
        case Lit(Boxed)
    }

    instance ToString[BodyTerm] {
        pub def toString(term: BodyTerm): String = match term {
            case BodyTerm.Wild        => "_"
            case BodyTerm.Var(varSym) => ToString.toString(varSym)
            case BodyTerm.Lit(v)      => Debug.stringify(v)
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // VarSym                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    pub enum VarSym with Eq, Order, ToString {
        case VarSym(String)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Fixity                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    pub enum Fixity {
        case Loose,
        case Fixed
    }

    /////////////////////////////////////////////////////////////////////////////
    // Polarity                                                                //
    /////////////////////////////////////////////////////////////////////////////

    pub enum Polarity {
        case Positive,
        case Negative
    }

}
