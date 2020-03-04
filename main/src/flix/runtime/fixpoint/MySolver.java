package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.LocalVariable;
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class MySolver {
    public static void solve(ConstraintSystem cs, Stratification stf, Options o) {
        ArrayList<RelSym> relHasFact = new ArrayList<>();
        Stmt[] factProjections = generateFactProjectionStmts(cs, relHasFact);

        Map<RelSym, ArrayList<Constraint>> derived = findRulesForDerived(cs);
        RelSym[] relSyms = cs.getRelationSymbols();
        // For representing all the RelSym that have rules but also initial facts
        ArrayList<RelSym> derivedButHasFacts = new ArrayList<>();
        // For representing the RelSym for tables that only have facts
        ArrayList<RelSym> factRelSyms = new ArrayList<>();
        for (RelSym relSym : relHasFact) {
            // We check if the RelSym that has a fact is also derived, and remember bot if and not
            if (derived.containsKey(relSym)) {
                derivedButHasFacts.add(relSym);
            } else {
                factRelSyms.add(relSym);
            }
        }
        for (RelSym relSym : derived.keySet()) {
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
        // Generate facts for each rule for the fact
        Stmt[][] acc = new Stmt[derived.get(relSym).size()][];
        ArrayList<Constraint> get = derived.get(relSym);
        for (int i = 0; i < get.size(); i++) {
            Constraint c = get.get(i);
            Map<PredSym, TableName> relTableMap = new HashMap<>();
            for (Predicate pred : c.getBodyPredicates()) {
                if (pred instanceof AtomPredicate) {
                    PredSym sym = ((AtomPredicate) pred).getSym();
                    // For all relation symbols we make a map to the TableName
                    if (sym instanceof RelSym) {
                        // Since we want to use all the facts, generated, we don't care if the facts are derived or not
                        relTableMap.put(sym, new TableName(TableClassifier.RESULT, sym));
                    } else {
                        throw new UnsupportedOperationException("We do not support LatSym yet");
                    }
                } else {
                    throw new UnsupportedOperationException("We only support AtomPredicates for now");
                }
            }
            // Evaluate each rule individually
            acc[i] = evalRule(c, relTableMap);
        }
        return Stream.of(acc).flatMap(Stream::of).toArray(Stmt[]::new);
    }

    private static Stmt[] evalRule(Constraint c, Map<PredSym, TableName> relTableMap) {
        Predicate head = c.getHeadPredicate();
        assert head instanceof AtomPredicate;
        PredSym headSym = ((AtomPredicate) head).getSym();
        Term[] headTerms = ((AtomPredicate) head).getTerms();
        // Map from AtomPredicate to the localvar used to get values to that recursion
        Map<AtomPredicate, LocalVariable> atomToLocal = new HashMap<>();
        for (Predicate bodyPred : c.getBodyPredicates()) {
            if (bodyPred instanceof AtomPredicate) {
                LocalVariable localVar = genNewLocalVariable(((AtomPredicate) bodyPred).getSym().getName());
                atomToLocal.put((AtomPredicate) bodyPred, localVar);
            } else {
                throw new UnsupportedOperationException("We only support AtomPredicates");
            }
        }
        return new Stmt[0];
    }

    private static LocalVariable genNewLocalVariable(String name) {
        throw new UnsupportedOperationException("Make actual new variables");
    }

    private static Map<RelSym, ArrayList<Constraint>> findRulesForDerived(ConstraintSystem cs) {
        Map<RelSym, ArrayList<Constraint>> result = new HashMap<>();
        for (Constraint c : cs.getRules()) {
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
    private static Stmt[] generateFactProjectionStmts(ConstraintSystem cs, ArrayList<RelSym> predHasFacts) {
        Stmt[] stmts = new Stmt[cs.getFacts().length];
        for (int factI = 0; factI < cs.getFacts().length; factI++) {
            Constraint c = cs.getFacts()[factI];
            assert c.getBodyPredicates().length == 0;

            Predicate pred = c.getHeadPredicate();
            assert pred instanceof AtomPredicate;

            AtomPredicate atom = (AtomPredicate) pred;
            PredSym predSym = atom.getSym();
            assert predSym instanceof RelSym;
            predHasFacts.add((RelSym) predSym);
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
