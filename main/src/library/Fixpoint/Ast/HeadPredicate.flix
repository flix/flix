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

namespace Fixpoint/Ast {
    pub enum HeadPredicate[v] {
        case HeadAtom(PredSym, Denotation[v], FooArray[HeadTerm[v]])
    }

    instance PredSymsOf[HeadPredicate[v]] {
        pub def predSymsOf(x: HeadPredicate[v]): Set[PredSym] = match x {
            case HeadAtom(predSym, _, _ ) => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[HeadPredicate[v]] {
        pub def substitute(x: HeadPredicate[v], s: Map[PredSym, PredSym]): HeadPredicate[v] = match x {
            case HeadAtom(predSym, den, terms) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                HeadAtom(newSym, den, terms)
        }
    }

    instance ToString[HeadPredicate[v]] with ToString[v] {
        pub def toString(head: HeadPredicate[v]): String =
            use Fixpoint.commaSeparate;
            match head {
                case HeadAtom(predSym, Relational, terms) => "${predSym}(${commaSeparate(terms)})"
                case HeadAtom(predSym, Latticenal(_), terms) =>
                    let len = Array.length(terms);
                    let k = terms[0..len-1];
                    let l = terms[len-1];
                    "${predSym}(${commaSeparate(k)}; ${l})"
            } as & Pure
    }
}
