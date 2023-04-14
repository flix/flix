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

namespace Fixpoint/Ast {

    use Fixpoint.PredSymsOf
    use Fixpoint.SubstitutePredSym
    use Fixpoint/Shared.PredSym
    use ToString.toString

    @Internal
    pub enum BodyPredicate[v] {
        case BodyAtom(PredSym, Denotation[v], Polarity, Fixity, Vector[BodyTerm[v]])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, VarSym)
        case Guard2(v -> v -> Bool, VarSym, VarSym)
        case Guard3(v -> v -> v -> Bool, VarSym, VarSym, VarSym)
        case Guard4(v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym)
        case Guard5(v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym)
        case Loop1(v -> Vector[v])
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
                    let (keyTerms, latticeTerms) = Vector.splitAt(Vector.length(terms)-1, terms);
                    match Vector.head(latticeTerms) {
                        case None    => "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}()"
                        case Some(l) => "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${keyTerms |> Vector.join(", ")}; ${l})"
                    }
                case BodyPredicate.Guard0(_) => "<clo>()"
                case BodyPredicate.Guard1(_, v) => "<clo>(${v})"
                case BodyPredicate.Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case BodyPredicate.Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case BodyPredicate.Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BodyPredicate.Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
                case BodyPredicate.Loop1(_) => "<loop>"
            }
    }
}
