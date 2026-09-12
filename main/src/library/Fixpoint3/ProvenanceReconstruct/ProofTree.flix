/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
