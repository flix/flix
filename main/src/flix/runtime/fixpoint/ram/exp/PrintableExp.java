package flix.runtime.fixpoint.ram.exp;

import java.io.PrintStream;

/**
 * The common printing interface for expressions
 */
public interface PrintableExp {
    /**
     * A function to print the statements as a program
     *
     * @param stream      The stram to print to
     */
    void prettyPrint(PrintStream stream);
}
