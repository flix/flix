package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.*;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;

public class MySolver {
    public static void solve(ConstraintSystem cs, Stratification stf, Options o) {
        Stmt[] factProjections = generateFactProjectionStmts(cs);

        SeqStmt seqStmt = new SeqStmt(factProjections);
        seqStmt.prettyPrint(System.out, 0);
    }

    /**
     * This method turns all Facts in the datalog program into projections into the initial relations
     *
     * @param cs Is the constraint system
     * @return an array of ProjectStmt representing the projection of the facts
     */
    private static Stmt[] generateFactProjectionStmts(ConstraintSystem cs) {
        Stmt[] stmts = new Stmt[cs.getFacts().length];
        for (int factI = 0; factI < cs.getFacts().length; factI++) {
            Constraint c = cs.getFacts()[factI];
            assert c.getBodyPredicates().length == 0;

            Predicate pred = c.getHeadPredicate();
            assert pred instanceof AtomPredicate;

            AtomPredicate atom = (AtomPredicate) pred;
            RamTerm[] ramTerms = new RamTerm[atom.getTerms().length];
            for (int termI = 0; termI < atom.getTerms().length; termI++) {
                Term term = atom.getTerms()[termI];
                assert term instanceof LitTerm;

                LitTerm litTerm = (LitTerm) term;
                ProxyObject proxy = litTerm.getFunction().apply(new Object[]{null});
                ramTerms[termI] = new flix.runtime.fixpoint.ram.LitTerm(proxy);
            }
            stmts[factI] = new ProjectStmt(ramTerms
                    , new TableName(TableClassifier.RESULT, atom.getSym()));
        }
        return stmts;
    }
}
