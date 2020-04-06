package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;
import flix.runtime.ReifiedSourceLocation;
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

public class MySolver {

    private static final Object[] nullArray = new Object[]{null};
    private static int variableCounter = 0;
    private static boolean addLabelStmts = true;

    public static void compileProgram(ConstraintSystem cs, Stratification stf, Options o) {
        ArrayList<RelSym> relHasFact = new ArrayList<>();

        // First we generate all projections for the facts
        ArrayList<Stmt> resultStmts = new ArrayList<>(Arrays.asList(generateFactProjectionStmts(cs, relHasFact)));

        // Then find all the rules and map them to what they derive, and in which stratum they should be evaluated
        Map<Integer, Map<RelSym, ArrayList<Constraint>>> derivedInStratum = findRulesForDerivedInStratums(cs, stf);

        // And then all stratum are evaluated
        resultStmts.addAll(compileStratums(derivedInStratum));

        SeqStmt seqStmt = new SeqStmt(resultStmts);
        PrintStream stream = System.out;
        seqStmt.prettyPrint(stream, 0);
        stream.print('\n');
    }

    /**
     * Responsible for compiling all stratums
     *
     * @param stratumMap A map describing relations and how they are derived in the individual stratum
     * @return An ArrayList of all statements in the compiled version of all stratums
     */
    private static ArrayList<Stmt> compileStratums(Map<Integer, Map<RelSym, ArrayList<Constraint>>> stratumMap) {
        ArrayList<Stmt> result = new ArrayList<>();
        for (int stratum : stratumMap.keySet()) {
            if (addLabelStmts) result.add(new LabelStmt("Stratum " + stratum));
            result.addAll(compileStratum(stratumMap.get(stratum)));
        }
        return result;
    }

    /**
     * Evaluates a stratum
     *
     * @param ruleMap Is a map to describe the constraints that can be used to derive new facts about the RelSym.
     * @return An array of all Stmt's that is the evaluation of the stratum
     */
    private static ArrayList<Stmt> compileStratum(Map<RelSym, ArrayList<Constraint>> ruleMap) {
        // First we compile the initialization of the algorithm where access to all facts is allowed
        ArrayList<Stmt> result = compileInit(ruleMap);
        if (addLabelStmts) result.add(new LabelStmt("Fixpoint starts here"));
        result.add(compileFixpoint(ruleMap));

        return result;
    }

    /**
     * Compiles the fixpoint part of the algorithm
     *
     * @param ruleMap A map from RelSym to a list of constraints where the constraints are assumed to be rules
     * @return A WhileStmt for the fixpoint
     */
    private static Stmt compileFixpoint(Map<RelSym, ArrayList<Constraint>> ruleMap) {
        BoolExp condition = compileWhileCondition(ruleMap.keySet());

        // First part of the while body consists of saving the DELTA tables into the NEW tables and clearing the DELTA table
        ArrayList<Stmt> whileBody = compileSaveLastIter(ruleMap.keySet());
        whileBody.addAll(compileClearLastIter(ruleMap.keySet()));

        // Then all relations are evaluated incrementally
        for (RelSym rel : ruleMap.keySet()) {
            whileBody.addAll(compileRulesIncr(ruleMap.get(rel)));
        }
        // And then the DELTA tables are saved into the RESULT tables
        whileBody.addAll(generateMergeStatements(ruleMap.keySet()));
        return new WhileStmt(condition, new SeqStmt(whileBody));
    }

    /**
     * Creates statements that clears Delta tables (meaning that they are set to the empty set)
     *
     * @param rels A set of RelSyms for the tables to be cleared
     * @return A list of statements
     */
    private static ArrayList<Stmt> compileClearLastIter(Set<RelSym> rels) {
        ArrayList<Stmt> result = new ArrayList<>();
        if (addLabelStmts)
            result.add(new LabelStmt("Clear Delta tables to make it ready for new facts from this iteration"));
        for (RelSym relSym : rels) {
            TableName delta = new TableName(TableVersion.DELTA, relSym);
            result.add(new AssignStmt(delta, new EmptyRelationExp()));
        }

        return result;
    }

    /**
     * Create statements that saves Delta tables in NEW tables (ie. ΔPath' = ΔPath)
     *
     * @param rels A set of RelSyms for the tables to be saved
     * @return A list of statements
     */
    private static ArrayList<Stmt> compileSaveLastIter(Set<RelSym> rels) {
        ArrayList<Stmt> result = new ArrayList<>();
        if (addLabelStmts) result.add(new LabelStmt("Remember facts generated in last iteration in NEW"));
        for (RelSym relSym : rels) {
            TableName mergeInto = new TableName(TableVersion.NEW, relSym);
            TableName delta = new TableName(TableVersion.DELTA, relSym);
            result.add(new AssignStmt(mergeInto, delta));
        }
        return result;
    }

    /**
     * Generates a BoolExp that tests whether the Delta tables are empty
     *
     * @param derived The set of RelSyms for which the Delta tables are tested
     * @return The compined BookExp
     */
    private static BoolExp compileWhileCondition(Set<RelSym> derived) {
        BoolExp result = null;
        for (RelSym relSym : derived) {
            TableName delta = new TableName(TableVersion.DELTA, relSym);
            if (result != null) {
                result = new OrBoolExp(result, new NotBoolExp(new EmptyBoolExp(delta)));
            } else {
                result = new NotBoolExp(new EmptyBoolExp(delta));
            }
        }
        return result;
    }

    /**
     * Generates a statements merging from a Delta table into a result table.
     *
     * @param relations A set of RelSyms where a merge is required for each of them.
     * @return A list with a Stmt for each merge
     */
    private static ArrayList<Stmt> generateMergeStatements(Set<RelSym> relations) {
        ArrayList<Stmt> result = new ArrayList<>();
        if (addLabelStmts) result.add(new LabelStmt("Merge new facts into result"));
        for (RelSym relSym : relations) {
            TableName mergeInto = new TableName(TableVersion.RESULT, relSym);
            RelationExp mergeExp = new BinaryRelationExp(BinaryRelationOperator.UNION,
                    mergeInto,
                    new TableName(TableVersion.DELTA, relSym));
            result.add(new AssignStmt(mergeInto, mergeExp));
        }
        return result;
    }

    /**
     * Compiles the initialization of a stratum using the all facts in the relations
     *
     * @param ruleMap A map from each relation to the rules to derive them
     * @return An ArrayList with all the Stmts of the compiled version
     */
    private static ArrayList<Stmt> compileInit(Map<RelSym, ArrayList<Constraint>> ruleMap) {
        ArrayList<Stmt> result = new ArrayList<>();
        if (addLabelStmts) result.add(new LabelStmt("Init"));
        for (RelSym relSym : ruleMap.keySet()) {
            result.addAll(compileRules(ruleMap.get(relSym)));
        }
        // The end of init all data is merged into the result tables
        result.addAll(generateMergeStatements(ruleMap.keySet()));
        return result;
    }

    /**
     * This method creates the statements for incremental evaluation of the rules
     *
     * @param constraints The constraints defining the rules for the relation
     * @return An array of statements evaluating the rules. Should be 1 ForEachStmt for each rule
     */
    private static ArrayList<Stmt> compileRulesIncr(ArrayList<Constraint> constraints) {
        ArrayList<Stmt> result = new ArrayList<>();
        for (Constraint constraint : constraints) {
            result.addAll(compileRuleIncr(constraint));
        }
        return result;
    }

    /**
     * This method generates one ForEachStmt for each RelSym used in the constraint, such that we look at the
     * knowledge earned from last iteration one by one
     *
     * @param constraint The specific constraint we want to generate Stmts for
     * @return The Stmts
     */
    private static ArrayList<Stmt> compileRuleIncr(Constraint constraint) {
        // Let's start by just dividing on AtomPredicate, TODO: we might need the rest of the bodyPredicate too
        Predicate headPredicate = constraint.getHeadPredicate();
        ArrayList<Stmt> result = new ArrayList<>();
        if (headPredicate instanceof AtomPredicate) {
            for (AtomPredicate bodyAtom : constraint.getBodyAtoms()) {
                if (addLabelStmts) {
                    ReifiedSourceLocation source = constraint.getSourceLocation();
                    String label;
                    if (source.getBeginLine() == source.getEndLine()) {
                        label = "Compilation of rule defined on line " + source.getBeginLine();
                    } else {
                        label = "Compilation of rule defined on lines " + source.getBeginLine() + " - " + source.getEndLine();
                    }
                    result.add(new LabelStmt(label + " with " + bodyAtom + " being used from previous iteration"));
                }
                result.add(compileRule(constraint, bodyAtom.getSym()));
            }
        } else {
            throw new IllegalArgumentException("The head of a constraint should be an AtomPredicate, right?");
        }

        return result;
    }

    /**
     * Vi har brug for at vi kan have en tuple som type datatype som hvor vi kan spørge om  eksistens i en tabel og indsætte i en tabel
     * Hvad nu hvis der optræder en literal (konstant) i Head eller body af en regel
     *
     * @param rules A list of constraints describing a set of rules to compile
     * @return An array of statements that should evaluate the rules deriving relSym
     */
    private static ArrayList<Stmt> compileRules(ArrayList<Constraint> rules) {
        // Generate facts for each rule for the fact
        ArrayList<Stmt> result = new ArrayList<>(rules.size());
        for (Constraint constraint : rules) {
            if (addLabelStmts) {
                ReifiedSourceLocation source = constraint.getSourceLocation();
                String label;
                if (source.getBeginLine() == source.getEndLine()) {
                    label = "Compilation of rule defined on line " + source.getBeginLine();
                } else {
                    label = "Compilation of rule defined on lines " + source.getBeginLine() + " - " + source.getEndLine();
                }
                result.add(new LabelStmt(label + " with all facts used"));
            }
            // Compile each rule individually
            result.add(compileRule(constraint, null));
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
    private static Stmt compileRule(Constraint c, PredSym newSym) {

        variableCounter = 0;

        Predicate head = c.getHeadPredicate();
        assert head instanceof AtomPredicate;
        PredSym headSym = ((AtomPredicate) head).getSym();
        Term[] headTerms = ((AtomPredicate) head).getTerms();

        // Map from AtomPredicate to the RowVariable used to get values to that recursion
        Map<AtomPredicate, RowVariable> atomToLocal = new HashMap<>();
        // Map from a variable to the set of AttrTerms they will be instantiated as
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
                    //TODO: Never add names from a negated predicate to varSymToAttrTerm
                 */
                if (currentPred.isNegative()) {
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
                    boolRestrictions.add(new NotBoolExp(new TupleInRelBoolExp(ramTerms, name)));
                } else {
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
        BoolExp checkBool = new NotBoolExp(new TupleInRelBoolExp(headRamTerms, new TableName(TableVersion.RESULT, headSym)));
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
        // I can now generate all the for each statements for all AtomPredicates.
        resultStmt = generateForEach(resultStmt, c.getBodyAtoms(), newSym, atomToLocal);


        return resultStmt;
    }

    /**
     * Generates a ForEach statement for each positive Atom. Each ForEach containing another, and the innermost containing 'body'
     * The first ForEach will match the last Atom in atoms.
     *
     * @param body        The body of the innermost ForEach generated
     * @param atoms       The Atoms for which the ForEach statements are made
     * @param newSym      The symbol that is in focus in the incremental part of the algorithm. If null ot does nothing
     * @param atomToLocal A map from the AtomPredicate to the RowVariable it will be represented with in the body
     * @return The resulting ForEachStmt
     */
    private static Stmt generateForEach(Stmt body, AtomPredicate[] atoms, PredSym newSym, Map<AtomPredicate, RowVariable> atomToLocal) {
        Stmt resultStmt = body;
        // Reversed for nicer printing. They now come in the same order as in the  rule
        for (int i = atoms.length - 1; i >= 0; i--) {
            AtomPredicate pred = atoms[i];

            // We do not need a ForEach when it's a negated Atom, since all possible values will be in other tables
            if (pred.isNegative()) continue;

            PredSym predSym = pred.getSym();
            TableName name;
            if (predSym == newSym) {
                name = new TableName(TableVersion.NEW, predSym);
            } else {
                name = new TableName(TableVersion.RESULT, predSym);
            }
            resultStmt = new ForEachStmt(name,
                    atomToLocal.get(pred),
                    resultStmt);
        }
        return resultStmt;
    }


    /**
     * Generates a new RowVariable using the variableCounter
     *
     * @param name The base name of the new RowVariable
     * @return A new RowVariable with name
     */
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
    private static Map<Integer, Map<RelSym, ArrayList<Constraint>>> findRulesForDerivedInStratums(ConstraintSystem cs, Stratification stf) {
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
