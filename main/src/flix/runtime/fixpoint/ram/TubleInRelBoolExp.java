package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class TubleInRelBoolExp implements BoolExp {
    RamTerm[] terms;
    TableName table;

    public TubleInRelBoolExp(RamTerm[] terms, TableName table) {
        this.terms = terms;
        this.table = table;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print("(");
        for (int i = 0; i < terms.length; i++) {
            stream.print(terms[i]);
            if (i < terms.length - 1){
                stream.print(", ");
            }
        }
        stream.print(") in ");
        table.prettyPrint(stream);
    }
}
