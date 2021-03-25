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

use Fixpoint/Ast.{PrecedenceGraph, PredSym};
use Fixpoint/Ast.PrecedenceGraph.PrecedenceGraph;
use Fixpoint/Ast.PrecedenceEdge.{PositiveEdge, NegativeEdge};

namespace Fixpoint {
    def stratify(g: PrecedenceGraph): Map[PredSym, Int32] = stratifyHelper(g, Map#{})

    /// Compute a stratification (if one exists) with Ullman's algorithm.
    def stratifyHelper(g: PrecedenceGraph, stf: Map[PredSym, Int32]): Map[PredSym, Int32] = match g {
        case PrecedenceGraph(xs) =>
            let maxStratum = Set.size(xs);
            let newStf = Set.foldLeft(acc -> edge -> match edge {
                case PositiveEdge(bodySym, headSym) =>
                    let bodyStratum = Map.getWithDefault(bodySym, 0, acc);
                    let headStratum = Map.getWithDefault(headSym, 0, acc);
                    if (bodyStratum > headStratum)
                        Map.insert(headSym, bodyStratum, acc)
                    else
                        Map.insertWith(Int32.max, headSym, 0, acc)
                case NegativeEdge(bodySym, headSym) =>
                    let bodyStratum = Map.getWithDefault(bodySym, 0, acc);
                    let headStratum = Map.getWithDefault(headSym, 0, acc);
                    if (bodyStratum >= headStratum)
                        let newHeadStratum = bodyStratum + 1;
                        if (newHeadStratum > maxStratum)
                            ???
                        else
                            Map.insert(headSym, newHeadStratum, acc)
                    else
                        Map.insertWith(Int32.max, headSym, 0, acc)
            }, stf, xs);
            if (newStf == stf)
                stf
            else
                stratifyHelper(g, newStf)
    }
}