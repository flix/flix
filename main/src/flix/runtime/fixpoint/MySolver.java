package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.RamTerm;
import flix.runtime.fixpoint.ram.TableClassifier;
import flix.runtime.fixpoint.ram.TableName;
import flix.runtime.fixpoint.ram.stmt.ProjectStmt;
import flix.runtime.fixpoint.ram.stmt.SeqStmt;
import flix.runtime.fixpoint.ram.stmt.Stmt;
import flix.runtime.fixpoint.symbol.PredSym;
import flix.runtime.fixpoint.symbol.RelSym;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class MySolver {
    public static void solve(ConstraintSystem cs, Stratification stf, Options o) {
        Stmt[] factProjections = generateFactProjectionStmts(cs);

        Map<RelSym, ArrayList<Constraint>> derived = findRulesForDerived(cs);
        RelSym[] relSyms = cs.getRelationSymbols();
        ArrayList<RelSym> notDerived = new ArrayList<>();
        for (RelSym relSym : relSyms) {
            // We check if the RelSym is derived by a rule, if not, we remember that
            if (!derived.containsKey(relSym)){
                notDerived.add(relSym);
            }
        }
        for (RelSym relSym : derived.keySet()){
            Stmt[] ramRule = eval(relSym, derived);
        }
        SeqStmt seqStmt = new SeqStmt(factProjections);
        seqStmt.prettyPrint(System.out, 0);
    }
/*
    Vi har brug for at vi kan have en tuble som type datatype som hvor vi kan spørge om  eksistens i en tabel og indsætte i en tabel
    Hvad nu hvis der optræder en literal (konstant) i Head eller body af en regel
 */
    private static Stmt[] eval(RelSym relSym, Map<RelSym, ArrayList<Constraint>> derived) {
        for(Constraint c : derived.get(relSym)){
            Predicate head = c.getHeadPredicate();
            assert head instanceof AtomPredicate;
            Term[] headTerms = ((AtomPredicate) head).getTerms();

            ArrayList<Pair<Term, Term>> pairs = new ArrayList<>();

        }
        return new Stmt[0];
    }

    private static Map<RelSym, ArrayList<Constraint>> findRulesForDerived(ConstraintSystem cs) {
        Map<RelSym, ArrayList<Constraint>> result = new HashMap<>();
        for (Constraint c : cs.getRules()){
            Predicate hPred = c.getHeadPredicate();
            assert hPred instanceof AtomPredicate;

            PredSym pred = ((AtomPredicate) hPred).getSym();
            assert pred instanceof RelSym;

            if (result.containsKey(pred)) {
                result.get(pred).add(c);
            } else {
                ArrayList<Constraint> list = new ArrayList<>();
                list.add(c);
                result.put((RelSym) pred, list);
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
