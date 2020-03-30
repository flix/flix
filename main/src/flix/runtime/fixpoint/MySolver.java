package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.RowVariable;
import flix.runtime.fixpoint.ram.exp.bool.*;
import flix.runtime.fixpoint.ram.exp.relation.*;
import flix.runtime.fixpoint.ram.stmt.*;
import flix.runtime.fixpoint.ram.term.AttrTerm;
import flix.runtime.fixpoint.ram.term.RamLitTerm;
import flix.runtime.fixpoint.ram.term.RamTerm;
import flix.runtime.fixpoint.symbol.PredSym;
import flix.runtime.fixpoint.symbol.RelSym;
import flix.runtime.fixpoint.symbol.VarSym;
import flix.runtime.fixpoint.term.AppTerm;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;
import flix.runtime.fixpoint.term.VarTerm;

import java.io.PrintStream;
import java.util.*;
import java.util.stream.Stream;

public class MySolver {

    private static final Object[] nullArray = new Object[]{null};
    private static int variableCounter = 0;

    public static void compileProgram(ConstraintSystem cs, Stratification stf, Options o) {
        ArrayList<RelSym> relHasFact = new ArrayList<>();

        // First we generate all projections for the facts
        Stmt[] factProjections = generateFactProjectionStmts(cs, relHasFact);

        // Then find all the rules and map them to what they derive, and in which stratum they should be evaluated
        Map<Integer, Map<RelSym, ArrayList<Constraint>>> derivedInStratum = findRulesForDerivedInStratum(cs, stf);

        // And then all stratum are evaluated
        Stmt[] stratumStmts = new Stmt[0];
        for (int stratum : derivedInStratum.keySet()) {
            Stmt[] fullStratum = evalStratum(derivedInStratum.get(stratum));
            stratumStmts = Stream.of(stratumStmts, fullStratum).flatMap(Stream::of).toArray(Stmt[]::new);
        }


        SeqStmt seqStmt = new SeqStmt(Stream.of(factProjections, stratumStmts).flatMap(Stream::of).toArray(Stmt[]::new));
        PrintStream stream = System.out;
        seqStmt.prettyPrint(stream, 0);
        stream.print('\n');
    }

    /**
     * Evaluates a stratum
     *
     * @param derived Is a map to describe the constraints that can be used to derive new facts about the RelSym.
     * @return An array of all Stmt's that is the evaluation of the stratum
     */
    private static Stmt[] evalStratum(Map<RelSym, ArrayList<Constraint>> derived) {
        Stmt[][] ramIntoRules = new Stmt[derived.keySet().size()][];
        Stmt[] mergeStmts = new Stmt[derived.keySet().size()];

        int i = 0;
        for (RelSym relSym : derived.keySet()) {
            ramIntoRules[i] = eval(relSym, derived);
            TableName orig = new TableName(TableVersion.RESULT, relSym);
            RelationExp mergeToOrig = new BinaryRelationExp(BinaryRelationOperator.UNION,
                    orig,
                    new TableName(TableVersion.DELTA, relSym));
            mergeStmts[i] = new AssignStmt(orig, mergeToOrig);
            i++;
        }
        Stmt[] ramRulesFlat = Stream.of(ramIntoRules).flatMap(Stream::of).toArray(Stmt[]::new);

        // Now We define the inner while loop that will be the main part of the algorithm

        // First we define the condition of the loop
        i = 0;
        Stmt[] saveLastIteration = new Stmt[derived.keySet().size()];
        Stmt[] clearLastIteration = new Stmt[derived.keySet().size()];
        BoolExp whileCondition = null;
        for (RelSym rel : derived.keySet()) {
            TableName mergeInto = new TableName(TableVersion.NEW, rel);
            TableName delta = new TableName(TableVersion.DELTA, rel);
            saveLastIteration[i] = new AssignStmt(mergeInto, delta);

            if (whileCondition != null) {
                whileCondition = new OrBoolExp(whileCondition, new NotBoolExp(new EmptyBoolExp(delta)));
            } else {
                whileCondition = new NotBoolExp(new EmptyBoolExp(delta));
            }

            // We should make sure that the tables holding the relations generated in an iteration is cleared at the start of each iteration
            clearLastIteration[i] = new AssignStmt(new TableName(TableVersion.DELTA, rel), new EmptyRelationExp());

            i++;
        }

        // Then we define the evaluation TODO: There is no real reason for 3 loops through the keyset, just for convenience while writing
        Stmt[] iterationEvaluation = new Stmt[0];
        for (RelSym rel : derived.keySet()) {
            Stmt[] eval = evalIncr(rel, derived.get(rel));
            iterationEvaluation = Stream.of(iterationEvaluation, eval).flatMap(Stream::of).toArray(Stmt[]::new);
        }


        SeqStmt whileBody = new SeqStmt(Stream.of(saveLastIteration, clearLastIteration, iterationEvaluation, mergeStmts).flatMap(Stream::of).toArray(Stmt[]::new));
        Stmt mainWhile = new WhileStmt(whileCondition, whileBody);

        return Stream.of(ramRulesFlat, mergeStmts, new Stmt[]{mainWhile}).flatMap(Stream::of).toArray(Stmt[]::new);
    }

    /**
     * This method creates the statements for incremental evaluation of the rules
     *
     * @param rel         The relation that is being evaluated
     * @param constraints The constraints defining the rules for the relation
     * @return An array of statements evaluating the rules. Should be 1 ForEachStmt for each rule
     */
    private static Stmt[] evalIncr(RelSym rel, ArrayList<Constraint> constraints) {
        Stmt[][] result = new Stmt[constraints.size()][];

        for (int i = 0; i < constraints.size(); i++) {
            Constraint constraint = constraints.get(i);
            result[i] = evalRuleIncr(constraint);
        }

        assert result.length == constraints.size();
        return Stream.of(result).flatMap(Stream::of).toArray(Stmt[]::new);
    }

    /**
     * This method generates one ForEachStmt for each RelSym used in the constraint, such that we look at the
     * knowledge earned from last iteration one by one
     *
     * @param constraint The specific constraint we want to generate Stmts for
     * @return The Stmts
     */
    private static Stmt[] evalRuleIncr(Constraint constraint) {
        // Let's start by just dividing on AtomPredicate, TODO: we might need the rest of the bodyPredicate too
        Predicate headPredicate = constraint.getHeadPredicate();
        Stmt[] result = new Stmt[constraint.getBodyAtoms().length];
        if (headPredicate instanceof AtomPredicate) {
            AtomPredicate[] bodyAtoms = constraint.getBodyAtoms();
            for (int i = 0; i < bodyAtoms.length; i++) {
                result[i] = evalRule(constraint, bodyAtoms[i].getSym());
            }
        } else {
            throw new IllegalArgumentException("The head of a constraint should be an AtomPredicate, right?");
        }

        return result;
    }

    /**
     * Vi har brug for at vi kan have en tuble som type datatype som hvor vi kan spørge om  eksistens i en tabel og indsætte i en tabel
     * Hvad nu hvis der optræder en literal (konstant) i Head eller body af en regel
     *
     * @param relSym  The RelSym we evaluate rules for
     * @param derived A map from RelSym to all the rules that is used to derive it
     * @return An array of statements that should evaluate the rules deriving relSym
     */
    private static Stmt[] eval(RelSym relSym, Map<RelSym, ArrayList<Constraint>> derived) {
        // Generate facts for each rule for the fact
        Stmt[] result = new Stmt[derived.get(relSym).size()];
        ArrayList<Constraint> get = derived.get(relSym);
        for (int i = 0; i < get.size(); i++) {
            Constraint c = get.get(i);
            Map<PredSym, TableName> relTableMap = new HashMap<>();
            // Evaluate each rule individually
            result[i] = evalRule(c, null);
        }
        return result;
    }

    /**
     * Lav et kodeeksempel i stil med https://github.com/flix/flix/blob/master/main/src/ca/uwaterloo/flix/language/phase/Synthesize.scala#L574
     *
     * @param c      The specific rule that we want to evaluate
     * @param newSym Is the symbol of the predicate where we access the data from previous iteration. In the base iteration this is null.
     * @return A statement evaluating c
     */
    private static Stmt evalRule(Constraint c, PredSym newSym) {

        variableCounter = 0;

        Predicate head = c.getHeadPredicate();
        assert head instanceof AtomPredicate;
        PredSym headSym = ((AtomPredicate) head).getSym();
        Term[] headTerms = ((AtomPredicate) head).getTerms();

        // Map from AtomPredicate to the localvar used to get values to that recursion
        Map<AtomPredicate, RowVariable> atomToLocal = new HashMap<>();
        // Map from terms to the set of AttrTerms they will be instantiated as
        Map<VarSym, Set<AttrTerm>> varSymToAttrTerm = new HashMap<>();
        // Define the set for all boolExps describing when a value must be constant
        Set<BoolExp> boolRestrictions = new HashSet<>();

        // Define all variables needed and define what the variables are instantiated as
        for (Predicate bodyPred : c.getBodyPredicates()) {
            if (bodyPred instanceof AtomPredicate) {
                AtomPredicate currentPred = (AtomPredicate) bodyPred;
                /*
                    If the AtomPredicate is a "not" predicate we need to generate an if-statement later that makes
                    sure that it holds. This must be done before we add the predicate to atomToLocal, since the values
                    cannot be determined by this predicate
                    TODO: Make sure that we do not traverse a negated predicate, since this is never necessary
                 */
                if (!currentPred.isPositive()) {
                    // This could perhaps be moved to the already existing loop across the terms
                    Term[] terms = currentPred.getTerms();
                    RamTerm[] ramTerms = new RamTerm[terms.length];
                    for (int i = 0; i < terms.length; i++) {
                        Term term = terms[i];
                        if (term instanceof VarTerm) {
                            VarSym sym = ((VarTerm) term).getSym();
                            ramTerms[i] = varSymToAttrTerm.get(sym).iterator().next();
                        } else if (term instanceof LitTerm) {
                            ramTerms[i] = new RamLitTerm(((LitTerm) term).getFunction().apply(nullArray));
                        } else {
                            throw new UnsupportedOperationException("Right now negated AtomPred can only contain" +
                                    "VarTerm or LitTerm, should at least also allow for WildTerm");
                        }
                    }
                    TableName name = new TableName(TableVersion.RESULT, currentPred.getSym());
                    boolRestrictions.add(new NotBoolExp(new TubleInRelBoolExp(ramTerms, name)));
                }

                // Since it is not negated we might have to traverse the values of this table
                RowVariable localVar = genNewRowVariable(currentPred.getSym().getName());
                atomToLocal.put(currentPred, localVar);
                Term[] terms = currentPred.getTerms();
                for (int i = 0; i < terms.length; i++) {
                    Term currentTerm = terms[i];
                    AttrTerm attrTerm = new AttrTerm(localVar, i);
                    // We now split on what kind of term it is.
                    if (currentTerm instanceof VarTerm) {
                        VarSym currentSym = ((VarTerm) currentTerm).getSym();
                        if (varSymToAttrTerm.containsKey(currentSym)) {
                            varSymToAttrTerm.get(currentSym).add(attrTerm);
                        } else {
                            HashSet<AttrTerm> attrSet = new HashSet<>();
                            attrSet.add(attrTerm);
                            varSymToAttrTerm.put(currentSym, attrSet);
                        }
                    } else if (currentTerm instanceof LitTerm) {
                        RamLitTerm literal = new RamLitTerm(((LitTerm) currentTerm).getFunction().apply(nullArray));
                        boolRestrictions.add(new EqualsBoolExp(attrTerm, literal));
                    } else if (currentTerm instanceof AppTerm) {
                        throw new UnsupportedOperationException("Right now the terms of predicates in the body" +
                                " of a rule cannot be AppTerm");
                        //TODO: Should it be possible? And what to do then?
                    }
                    //TODO: Is it necessary to do something with WildTerm?
                }
            } else {
                throw new UnsupportedOperationException("We only support AtomPredicates");
            }
        }
        // I can now generate the project statement since the values of each term is known
        RamTerm[] headRamTerms = new RamTerm[headTerms.length];
        for (int i = 0; i < headTerms.length; i++) {
            Term term = headTerms[i];
            if (term instanceof VarTerm) {
                VarSym sym = ((VarTerm) term).getSym();
                Set<AttrTerm> a = varSymToAttrTerm.get(sym);
                headRamTerms[i] = varSymToAttrTerm.get(sym).iterator().next();
            } else if (term instanceof LitTerm) {
                headRamTerms[i] = new RamLitTerm(((LitTerm) term).getFunction().apply(nullArray));
            }

        }
        Stmt resultStmt = new ProjectStmt(headRamTerms, new TableName(TableVersion.DELTA, headSym));
        // Now I need to check that this element does not exist already
        BoolExp checkBool = new NotBoolExp(new TubleInRelBoolExp(headRamTerms, new TableName(TableVersion.RESULT, headSym)));
        resultStmt = new IfStmt(checkBool, resultStmt);

        // I can then generate the list of if statements
        for (VarSym sym : varSymToAttrTerm.keySet()) {
            Set<AttrTerm> attrs = varSymToAttrTerm.get(sym);

            Iterator<AttrTerm> it = attrs.iterator();
            AttrTerm first = it.next();
            while (it.hasNext()) {
                AttrTerm attr = it.next();
                BoolExp equalsBool = new EqualsBoolExp(first, attr);
                resultStmt = new IfStmt(equalsBool, resultStmt);
            }
        }
        for (BoolExp exp : boolRestrictions) {
            resultStmt = new IfStmt(exp, resultStmt);
        }

        // I can now generate all the for each statements for all AtomPredicates
        for (Predicate pred : c.getBodyPredicates()) {
            if (pred instanceof AtomPredicate) {
                AtomPredicate currentPred = (AtomPredicate) pred;
                PredSym predSym = currentPred.getSym();
                TableName name;
                if (predSym == newSym) {
                    name = new TableName(TableVersion.NEW, predSym);
                } else {
                    name = new TableName(TableVersion.RESULT, predSym);
                }
                resultStmt = new ForEachStmt(name,
                        atomToLocal.get(currentPred),
                        resultStmt);
            }
        }

        return resultStmt;
    }

    private static RowVariable genNewRowVariable(String name) {
        variableCounter++;
        return new RowVariable(name + "_" + (variableCounter));
    }

    /**
     * This functions generates a map from each stratum to RelSyms that must be evaluated in that stratum.
     * And then for each RelSym is a map to the rules for generating facts for it.
     *
     * @param cs  The ConstraintSystem. Her only the rules of it are used
     * @param stf A Stratification used to find out which stratum each relation is part of
     * @return A Map from stratum to a map from relations to the rules to derive them
     */
    private static Map<Integer, Map<RelSym, ArrayList<Constraint>>> findRulesForDerivedInStratum(ConstraintSystem cs, Stratification stf) {
        Map<Integer, Map<RelSym, ArrayList<Constraint>>> result = new HashMap<>();
        for (Constraint c : cs.getRules()) {
            Predicate hPred = c.getHeadPredicate();
            assert hPred instanceof AtomPredicate;

            PredSym pred = ((AtomPredicate) hPred).getSym();
            assert pred instanceof RelSym;
            int stratum = stf.getStratum(pred);
            if (result.containsKey(stratum) && result.get(stratum) != null) {
                Map<RelSym, ArrayList<Constraint>> map = result.get(stratum);
                if (map.containsKey(pred)) {
                    map.get(pred).add(c);
                } else {
                    ArrayList<Constraint> list = new ArrayList<>();
                    list.add(c);
                    map.put((RelSym) pred, list);
                }

            } else {
                Map<RelSym, ArrayList<Constraint>> newMap = new HashMap<>();
                ArrayList<Constraint> constraints = new ArrayList<>();
                constraints.add(c);
                newMap.put((RelSym) pred, constraints);
                result.put(stratum, newMap);
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
                ProxyObject proxy = litTerm.getFunction().apply(nullArray);
                ramTerms[termI] = new RamLitTerm(proxy);
            }
            stmts[factI] = new ProjectStmt(ramTerms
                    , new TableName(TableVersion.RESULT, atom.getSym()));
        }
        return stmts;
    }
}
