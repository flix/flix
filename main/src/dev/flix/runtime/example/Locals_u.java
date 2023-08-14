package dev.flix.runtime.example;

/**
 * An object which holds the PC + local variables of `def u`.
 */
public class Locals_u { // Aka. "FrameData".
    public final int pc;

    public final String name;
    public final String greetings;

    public Locals_u(int pc, String name, String greetings) {
        this.pc = pc;
        this.name = name;
        this.greetings = greetings;
    }
}
