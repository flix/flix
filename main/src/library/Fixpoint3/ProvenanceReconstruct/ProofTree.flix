/*
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

pub mod Fixpoint3.ProvenanceReconstruct.ProofTree {
    use Fixpoint3.ProvenanceReconstruct.ProofTree

    pub def toString(p: ProofTree[r], indentChild: Int32): String \ r = match p {
        case EDB(pred, fact) =>
            let indentCur = String.repeat(indentChild, " ");
            let factString = Vector.join(", ", fact);
            "${indentCur}${pred}(${factString}). Fact"
        case Negative(pred, fact) =>
            let indentCur = String.repeat(indentChild, " ");
            let factString = Vector.join(", ", fact);
            "${indentCur}No ${pred}(${factString})"
        case IDB(pred, fact, subs) =>
            let factString = Vector.join(", ", fact);
            let indentCur = String.repeat(indentChild, " ");
            let subProofs = Array.joinWith(sub -> toString(sub, indentChild + 4), "${String.lineSeparator()}", subs);
            "${indentCur}${pred}(${factString}). Proof:${String.lineSeparator()}${subProofs}"
    }
}
