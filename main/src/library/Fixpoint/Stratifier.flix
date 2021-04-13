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

use Fixpoint/Ast.{Datalog, PrecedenceGraph, PredSym};
use Fixpoint/Ast.Datalog.Datalog;
use Fixpoint/Ast.PrecedenceGraph.PrecedenceGraph;
use Fixpoint/Ast.PrecedenceEdge.{PositiveEdge, NegativeEdge};

namespace Fixpoint {
    ///
    /// Compute a stratification with Ullman's algorithm.
    /// The Flix compiler is supposed to guarantee the existence of a stratification at this point.
    /// Initially, all IDB predicates are assigned to stratum 0.
    /// I.e. facts are ignored in the stratification.
    ///
    def stratify(d: Datalog[v]): Map[PredSym, Int32] = match d {
        case Datalog(_, rules) =>
            use Fixpoint/Ast.mkDepGraph;
            predicatesOf(rules) |>
            Set.foldRight(p -> Map.insert(p, 0), Map#{}) |>
            stratifyHelper(mkDepGraph(d))
    }

    def stratifyHelper(g: PrecedenceGraph, stf: Map[PredSym, Int32]): Map[PredSym, Int32] = match g {
        case PrecedenceGraph(xs) =>
            // The number of strata is bounded by the number of predicates
            // which is bounded by the number of edges in the precedence graph.
            let maxStratum = Set.size(xs);
            // Visit every edge and ensure the following:
            // 1. If (body, head) is a positive edge, then body belongs to a lower stratum
            // or the same stratum as head.
            // 2. If (body, head) is a negative edge, then body belongs to a strictly lower stratum
            // than head.
            let (newStf, changed) = Set.foldLeft(match (acc, changed) -> edge -> match edge {
                case PositiveEdge(bodySym, headSym) =>
                    let bodyStratum = Map.getWithDefault(bodySym, 0, acc);
                    let headStratum = Map.getWithDefault(headSym, 0, acc);
                    if (bodyStratum > headStratum)
                        (Map.insert(headSym, bodyStratum, acc), true)
                    else
                        (acc, changed)
                case NegativeEdge(bodySym, headSym) =>
                    let bodyStratum = Map.getWithDefault(bodySym, 0, acc);
                    let headStratum = Map.getWithDefault(headSym, 0, acc);
                    if (bodyStratum >= headStratum)
                        let newHeadStratum = bodyStratum + 1;
                        // If there are more strata than edges,
                        // the precedence graph must contain a negative cycle!
                        if (newHeadStratum > maxStratum)
                            bug!("Stratification error (negative cycle)")
                        else
                            (Map.insert(headSym, newHeadStratum, acc), true)
                    else
                        (acc, changed)
            }, (stf, false), xs);
            // Check if property 1 and 2 now holds for all edges.
            if (changed)
                stratifyHelper(g, newStf)
            else
                stf
    }
}