package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.*;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;

public class MySolver {
    public static void solve(ConstraintSystem cs, Stratification stf, Options o) {
        Stmt[] stmts = new Stmt[cs.getFacts().length];
        for (int i = 0; i < cs.getFacts().length; i++) {
            Constraint c = cs.getFacts()[i];
            assert c.getBodyPredicates().length == 0;

            Predicate pred = c.getHeadPredicate();
            assert pred instanceof AtomPredicate;

            AtomPredicate atom = (AtomPredicate) pred;
            RamTerm[] ramTerms = new RamTerm[atom.getTerms().length];
            for (int j = 0; j < atom.getTerms().length; j++) {
                Term term = atom.getTerms()[i];
                assert term instanceof LitTerm;

                LitTerm litTerm = (LitTerm) term;
                ProxyObject proxy = litTerm.getFunction().apply(null);
                ramTerms[i] = new flix.runtime.fixpoint.ram.LitTerm(proxy);
            }
            stmts[i] = new ProjectStmt(ramTerms
                    , new TableName(TableClassifier.RESULT, atom.getSym()));
        }
        SeqStmt seqStmt = new SeqStmt(stmts);
        seqStmt.prettyPrint(System.out, 0);
    }
}
