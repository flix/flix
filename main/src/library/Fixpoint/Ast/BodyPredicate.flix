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
    pub enum BodyPredicate[v] {
        case BodyAtom(PredSym, Polarity, List[BodyTerm[v]], SourceLocation)
    }

    def bodyVars(body: List[BodyPredicate[v]]): Set[VarSym] = match polarityVars(body) {
        case (p, n) => Set.union(p, n)
    }

    def polarityVars(body: List[BodyPredicate[v]]): (Set[VarSym], Set[VarSym]) =
        List.foldLeft((acc, bp) -> match (acc, bp) {
            case ((p, n), BodyAtom(_, Positive, terms, _)) => (Set.union(p, btermVars(terms)), n)
            case ((p, n), BodyAtom(_, Negative, terms, _)) => (p, Set.union(n, btermVars(terms)))
        }, (Set.empty(), Set.empty()), body)

    def btermVars(terms: List[BodyTerm[v]]): Set[VarSym] =
        List.foldLeft((acc, t) -> match t {
            case BodyTerm.Var(var, _) => Set.insert(var, acc)
            case _ => acc
        }, Set.empty(), terms)

    instance ToString[BodyPredicate[v]] with [v : ToString] {
        def toString(body: BodyPredicate[v]): String = match body {
            case BodyAtom(predSym, Positive, terms, _) =>
                let ts = List.map(ToString.toString, terms) |> String.intercalate(", ");
                "${predSym}(${ts})"
            case BodyAtom(predSym, Negative, terms, _) =>
                let ts = List.map(ToString.toString, terms) |> String.intercalate(", ");
                "not ${predSym}(${ts})"
        }
    }
}
