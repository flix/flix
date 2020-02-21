package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

/**
 * The common interface for RAM relation expressions
 */
public interface RelationExp {
    /**
     * A function to print the statements as a program
     *
     * @param stream      The stram to print to
     * @param indentation The amount of indentation to put before printing
     */
    void prettyPrint(PrintStream stream, int indentation);
}
