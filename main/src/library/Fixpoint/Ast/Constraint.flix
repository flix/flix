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
use Fixpoint/Shared.PredSym;

namespace Fixpoint/Ast {
    pub enum Constraint[v] {
        case Constraint(HeadPredicate[v], Array[BodyPredicate[v]])
    }

    instance PredSymsOf[Constraint[v]] {
        pub def predSymsOf(x: Constraint[v]): Set[PredSym] = match x {
            case Constraint(head, body) =>
                let headSyms = predSymsOf(head);
                let bodySyms = Array.map(predSymsOf, body) |> Array.toList;
                headSyms ++ Monoid.fold(bodySyms)
        } as & Pure
    }

    instance ToString[Constraint[v]] with ToString[v] {
        pub def toString(c: Constraint[v]): String =
            use Fixpoint.commaSeparate;
            match c {
                case Constraint(head, body) =>
                    if (Array.length(body) == 0)
                        "${head}."
                    else
                        "${head} :- ${commaSeparate(body)}."
            } as & Pure
    }
}
