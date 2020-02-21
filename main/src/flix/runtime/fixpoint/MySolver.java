package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.*;
import flix.runtime.fixpoint.symbol.PredSym;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class MySolver {
    public static void solve(ConstraintSystem cs, Stratification stf, Options o) {
        Stmt[] factProjections = generateFactProjectionStmts(cs);
        Map<PredSym, ArrayList<Constraint>>derived = findRulesForDerived(cs);
        //Stmt[] main_loop_stmts = generateRuleStmts(cs, stf);

        SeqStmt seqStmt = new SeqStmt(factProjections);
        seqStmt.prettyPrint(System.out, 0);
    }

    private static Map<PredSym, ArrayList<Constraint>> findRulesForDerived(ConstraintSystem cs) {
        Map<PredSym, ArrayList<Constraint>> result = new HashMap<>();
        for (Constraint c : cs.getRules()){
            Predicate hPred = c.getHeadPredicate();
            assert hPred instanceof AtomPredicate;

            PredSym pred = ((AtomPredicate) hPred).getSym();
            if (result.containsKey(pred)) {
                result.get(pred).add(c);
            } else {
                ArrayList<Constraint> list = new ArrayList<>();
                list.add(c);
                result.put(pred, list);
            }
        }
        return result;
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
