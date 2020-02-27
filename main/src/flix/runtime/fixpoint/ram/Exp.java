package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

/**
 * The common interface for all expressions
 */
public interface Exp {
    /**
     * A function to print the statements as a program
     *
     * @param stream      The stram to print to
     */
    void prettyPrint(PrintStream stream);
}
