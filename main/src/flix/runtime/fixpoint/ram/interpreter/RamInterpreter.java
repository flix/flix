package flix.runtime.fixpoint.ram.interpreter;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.Constraint;
import flix.runtime.fixpoint.ConstraintSystem;
import flix.runtime.fixpoint.ram.RowVariable;
import flix.runtime.fixpoint.ram.exp.bool.BoolExp;
import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.ram.exp.relation.TableName;
import flix.runtime.fixpoint.ram.stmt.*;
import flix.runtime.fixpoint.ram.term.AttrTerm;
import flix.runtime.fixpoint.ram.term.RamLitTerm;
import flix.runtime.fixpoint.ram.term.RamTerm;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class RamInterpreter {

    private static boolean printNewFacts = false;
    private static PrintStream printStream = System.out;

    public static ConstraintSystem run(Stmt toInterpret) {
        Map<TableName, Set<Fact>> interpretation = new HashMap<>();
        Map<RowVariable, Fact> environment = new HashMap<>();

        evalStmt(toInterpret, interpretation, environment);
        Constraint[] result = new Constraint[0];
        interpretation.size();
        for (TableName table : interpretation.keySet()) {
            Set<Fact> tableFacts = interpretation.get(table);
            for (Fact fact : tableFacts) {

            }
        }

        return ConstraintSystem.of(new Constraint[0]);
    }

    private static void evalStmt(Stmt toEval, Map<TableName, Set<Fact>> interpretation, Map<RowVariable, Fact> environment) {
        // To interpret an AssignStmt the TableName is overwritten with what an interpretation of the RelationExp returns
        if (toEval instanceof AssignStmt) {
            AssignStmt currStmt = (AssignStmt) toEval;
            Set<Fact> relationFacts = evalRelationExp(currStmt.getRelationExp(), interpretation, environment);
            interpretation.put(currStmt.getName(), relationFacts);
        } else if (toEval instanceof ForEachStmt) {
            ForEachStmt currStmt = (ForEachStmt) toEval;
            // To evaluate a ForEachStmt we have to go through all the elements in the corresponding table, evaluating the body with the new corresponding environment
            for (Fact fact : interpretation.get(currStmt.getName())) {
                environment.put(currStmt.getLocalVar(), fact);
                evalStmt(currStmt.getBody(), interpretation, environment);
            }
            environment.remove(currStmt.getLocalVar());
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
            if (interpretation.get(currStmt.getTable()).add(newFact) && printNewFacts) {
                printStream.println("Added the fact:" + newFact + " to: " + currStmt.getTable());
            }
        } else if (toEval instanceof SeqStmt) {
            for (Stmt stmt : ((SeqStmt) toEval).getStmts()) {
                evalStmt(stmt, interpretation, environment);
            }
        } else if (toEval instanceof WhileStmt) {
            WhileStmt currStmt = (WhileStmt) toEval;
            while (evalBoolExp(currStmt.getCondition(), interpretation, environment)) {
                evalStmt(currStmt.getBody(), interpretation, environment);
            }
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
        return false;
    }

    private static Set<Fact> evalRelationExp(RelationExp toEval, Map<TableName, Set<Fact>> interpretation, Map<RowVariable, Fact> environment) {
        return null;
    }

}
