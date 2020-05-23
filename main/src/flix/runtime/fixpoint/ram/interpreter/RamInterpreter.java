package flix.runtime.fixpoint.ram.interpreter;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.Constraint;
import flix.runtime.fixpoint.ConstraintSystem;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.RowVariable;
import flix.runtime.fixpoint.ram.exp.bool.*;
import flix.runtime.fixpoint.ram.exp.relation.EmptyRelationExp;
import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.ram.exp.relation.TableName;
import flix.runtime.fixpoint.ram.exp.relation.UnionRelationExp;
import flix.runtime.fixpoint.ram.stmt.*;
import flix.runtime.fixpoint.ram.term.AttrTerm;
import flix.runtime.fixpoint.ram.term.RamLitTerm;
import flix.runtime.fixpoint.ram.term.RamTerm;
import flix.runtime.fixpoint.symbol.VarSym;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;

import java.io.PrintStream;
import java.util.*;

public class RamInterpreter {

    private static final boolean printNewFacts = false;
    private static final PrintStream printStream = System.out;

    public static ConstraintSystem run(Stmt toInterpret) {
        Map<TableName, Set<Fact>> interpretation = new HashMap<>();
        Map<RowVariable, Fact> environment = new HashMap<>();

        evalStmt(toInterpret, interpretation, environment);
        ArrayList<Constraint> resultConstraints = new ArrayList<>();
        for (TableName table : interpretation.keySet()) {
            if (!interpretation.containsKey(table)) {
                printStream.println("Something wrong");
            }
            Set<Fact> tableFacts = interpretation.get(table);
            if (tableFacts == null) {
                printStream.print("No fact set for table ");
                table.prettyPrint(printStream, 0);
                printStream.print('\n');
            } else {
                for (Fact fact : tableFacts) {
                    Term[] terms = factToTermArray(fact);
                    AtomPredicate headPredicate = AtomPredicate.of(table.getName(),true, terms);
                    resultConstraints.add(Constraint.of(new VarSym[0], headPredicate, new Predicate[0], null));
                }
            }
        }

        return ConstraintSystem.of(resultConstraints.toArray(Constraint[]::new));
    }

    private static Term[] factToTermArray(Fact fact) {
        Term[] result = new Term[fact.factSize()];
        for (int i = 0; i < fact.factSize(); i++) {
            ProxyObject object = fact.getElement(i);
            result[i] = LitTerm.of((o) -> object);
        }
        return result;
    }

    private static void evalStmt(Stmt toEval, Map<TableName, Set<Fact>> interpretation, Map<RowVariable, Fact> environment) {
        // To interpret an AssignStmt the TableName is overwritten with what an interpretation of the RelationExp returns
        if (toEval instanceof AssignStmt) {
            AssignStmt currStmt = (AssignStmt) toEval;
            Set<Fact> relationFacts = evalRelationExp(currStmt.getRelationExp(), interpretation, environment);
            interpretation.put(currStmt.getName(), relationFacts);
        } else if (toEval instanceof ForEachStmt) {
            ForEachStmt currStmt = (ForEachStmt) toEval;
            TableName currTable = currStmt.getName();
            // To evaluate a ForEachStmt we have to go through all the elements in the corresponding table, evaluating the body with the new corresponding environment
            if (!interpretation.containsKey(currTable)) {
                interpretation.put(currTable, new LinkedHashSet<>());
            } else {
                for (Fact fact : interpretation.get(currStmt.getName())) {
                    environment.put(currStmt.getLocalVar(), fact);
                    evalStmt(currStmt.getBody(), interpretation, environment);
                }
                environment.remove(currStmt.getLocalVar());
            }

        } else if (toEval instanceof IfStmt) {
            IfStmt currStmt = (IfStmt) toEval;
            if (evalBoolExp(currStmt.getGuard(), interpretation, environment)) {
                evalStmt(currStmt.getBodyStmt(), interpretation, environment);
            }
        } else if (toEval instanceof ProjectStmt) {
            ProjectStmt currStmt = (ProjectStmt) toEval;
            RamTerm[] facts = currStmt.getFacts();
            ProxyObject[] factValues = new ProxyObject[facts.length];
            for (int i = 0; i < facts.length; i++) {
                RamTerm term = facts[i];
                factValues[i] = evalRamTerm(term, interpretation, environment);
            }
            Fact newFact = new Fact(factValues);
            addFact(newFact, currStmt.getTable(), interpretation);

        } else if (toEval instanceof SeqStmt) {
            for (Stmt stmt : ((SeqStmt) toEval).getStmts()) {
                evalStmt(stmt, interpretation, environment);
            }
        } else if (toEval instanceof WhileStmt) {
            WhileStmt currStmt = (WhileStmt) toEval;
            while (evalBoolExp(currStmt.getCondition(), interpretation, environment)) {
                evalStmt(currStmt.getBody(), interpretation, environment);
            }
        } else if (!(toEval instanceof LabelStmt)) {
            throw new IllegalArgumentException(toEval.getClass().getName() + " is not a supported argument for the interpreter.");
        }
    }

    private static void addFact(Fact newFact, TableName addToTable, Map<TableName, Set<Fact>> interpretation) {
        if (!interpretation.containsKey(addToTable)) {
            Set<Fact> newSet = new LinkedHashSet<>();
            interpretation.put(addToTable, newSet);
        }
        if (interpretation.get(addToTable).add(newFact) && printNewFacts) {
            printStream.print("Added the fact:" + newFact + " to: ");
            addToTable.prettyPrint(printStream, 0);
            printStream.print('\n');
        }

    }

    private static ProxyObject evalRamTerm(RamTerm toEval, Map<TableName, Set<Fact>> interpretation, Map<RowVariable, Fact> environment) {
        if (toEval instanceof RamLitTerm) {
            return ((RamLitTerm) toEval).getLiteral();
        } else if (toEval instanceof AttrTerm) {
            AttrTerm currTerm = (AttrTerm) toEval;
            return environment.get(currTerm.getLocalVar()).getElement(currTerm.getIndex());
        }
        return null;
    }

    private static boolean evalBoolExp(BoolExp toEval, Map<TableName, Set<Fact>> interpretation, Map<RowVariable, Fact> environment) {
        if (toEval instanceof AndBoolExp) {
            AndBoolExp andExp = (AndBoolExp) toEval;
            if (evalBoolExp(andExp.getLeftExp(), interpretation, environment)) {
                return evalBoolExp(andExp.getRightExp(), interpretation, environment);
            } else {
                return false;
            }
        } else if (toEval instanceof EmptyBoolExp) {
            EmptyBoolExp emptyExp = (EmptyBoolExp) toEval;
            Set<Fact> res = evalRelationExp(emptyExp.getRelExp(), interpretation, environment);
            assert res != null;
            return res.isEmpty();
        } else if (toEval instanceof EqualsBoolExp) {
            EqualsBoolExp equalsExp = (EqualsBoolExp) toEval;
            ProxyObject term1 = evalRamTerm(equalsExp.getTerm1(), interpretation, environment);
            ProxyObject term2 = evalRamTerm(equalsExp.getTerm2(), interpretation, environment);
            assert term1 != null;
            assert term2 != null;
            return term1.equals(term2);
        } else if (toEval instanceof NotBoolExp) {
            NotBoolExp notExp = (NotBoolExp) toEval;
            return !evalBoolExp(notExp.getExp(), interpretation, environment);
        } else if (toEval instanceof OrBoolExp) {
            OrBoolExp orExp = (OrBoolExp) toEval;
            if (evalBoolExp(orExp.getLeftExp(), interpretation, environment)) {
                return true;
            } else return evalBoolExp(orExp.getRightExp(), interpretation, environment);
        } else if (toEval instanceof TupleInRelBoolExp) {
            TupleInRelBoolExp tupleInRelExp = (TupleInRelBoolExp) toEval;
            Set<Fact> relFacts = evalRelationExp(tupleInRelExp.getExp(), interpretation, environment);
            RamTerm[] terms = tupleInRelExp.getTerms();
            ProxyObject[] objects = new ProxyObject[terms.length];
            for (int i = 0; i < terms.length; i++) {
                objects[i] = evalRamTerm(terms[i], interpretation, environment);
            }
            assert relFacts != null;
            return relFacts.contains(new Fact(objects));
        }
        return false;
    }

    private static Set<Fact> evalRelationExp(RelationExp toEval, Map<TableName, Set<Fact>> interpretation, Map<RowVariable, Fact> environment) {
        if (toEval instanceof EmptyRelationExp) {
            return new LinkedHashSet<>();
        } else if (toEval instanceof TableName) {
            TableName currTable = (TableName) toEval;
            if (!interpretation.containsKey(toEval)) {
                interpretation.put(currTable, new LinkedHashSet<>());
            }
            return interpretation.get(toEval);
        } else if (toEval instanceof UnionRelationExp) {
            UnionRelationExp currExp = (UnionRelationExp) toEval;
            Set<Fact> eval1 = evalRelationExp(currExp.getExp1(), interpretation, environment);
            Set<Fact> eval2 = evalRelationExp(currExp.getExp2(), interpretation, environment);
            assert eval1 != null;
            if (eval1.isEmpty()) return eval2;
            assert eval2 != null;
            if (eval2.isEmpty()) return eval1;
            if (!eval1.addAll(eval2)) {
                printStream.println("Could not union");
            }
            return eval1;
        }

        return null;
    }

}
