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
    use Fixpoint.SubstitutePredSym
    use Fixpoint.Shared.PredSym

    @internal
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
}
