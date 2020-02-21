package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

/**
 * Common interface for Statements in the RAM language
 */
public interface Stmt {
    /**
     * A function to print the statements as a program
     *
     * @param stream      The stram to print to
     * @param indentLevel The amount of indentation to put before printing
     */
    void prettyPrint(PrintStream stream, int indentLevel);
}
