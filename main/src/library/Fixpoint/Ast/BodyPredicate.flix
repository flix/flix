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
use Fixpoint.SubstitutePredSym;
use Fixpoint/Shared.PredSym;
use ToString.toString;

namespace Fixpoint/Ast {
    pub enum BodyPredicate[v] {
        case BodyAtom(PredSym, Denotation[v], Polarity, Fixity, List[BodyTerm[v]])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, VarSym)
        case Guard2(v -> v -> Bool, VarSym, VarSym)
        case Guard3(v -> v -> v -> Bool, VarSym, VarSym, VarSym)
        case Guard4(v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym)
        case Guard5(v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym)
        case Loop1(v -> Array[v, Static])
    }

    instance PredSymsOf[BodyPredicate[v]] {
        pub def predSymsOf(x: BodyPredicate[v]): Set[PredSym] = match x {
            case BodyAtom(predSym, _, _, _, _) => Set.singleton(predSym)
            case _                             => Set.empty()
        }
    }

    instance SubstitutePredSym[BodyPredicate[v]] {
        pub def substitute(x: BodyPredicate[v], s: Map[PredSym, PredSym]): BodyPredicate[v] = match x {
            case BodyAtom(predSym, den, polarity, fixity, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                BodyAtom(newSym, den, polarity, fixity, terms)
            case _                             => x
        }
    }

    instance ToString[BodyPredicate[v]] with ToString[v] {
        pub def toString(body: BodyPredicate[v]): String =
            use Fixpoint.commaSeparate;
            def polarityPrefix(p) = match p {
                case Negative => "not "
                case Positive => ""
            };
            def fixityPrefix(f) = match f {
                case Fixed => "fix "
                case Loose => ""
            };
            match body {
                case BodyAtom(predSym, Relational, p, f, terms) =>
                    "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${terms |> List.map(toString) |> String.intercalate(", ")})"
                case BodyAtom(predSym, Latticenal(_), p, f, terms) =>
                    let len = List.length(terms);
                    let (k, lList) = List.splitAt(len-1, terms);
                    let l = List.head(lList) |> Option.map(toString) |> Option.getWithDefault("?");
                    "${polarityPrefix(p)}${fixityPrefix(f)}${predSym}(${k |> List.map(toString) |> String.intercalate(", ")}; ${l})"
                case Guard0(_) => "<clo>()"
                case Guard1(_, v) => "<clo>(${v})"
                case Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
                case Loop1(_) => "<loop>"
            }
    }
}