package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

/**
 * The common printing interface for expressions
 */
public interface Printable {
    /**
     * A function to print the statements as a program
     *
     * @param stream      The stream to print to
     * @param indentLevel The amount of indentations before writing a line
     */
    void prettyPrint(PrintStream stream, int indentLevel);
}
