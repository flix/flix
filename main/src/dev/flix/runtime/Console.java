package dev.flix.runtime;

public interface Console {
    // Result read(Locals_?, Kont?);
    Result print(String s);
}

class ConsoleHandler17 implements Console {
    public Result print(String s) {
        return null;
    }
}

// and then we can create objects/instances for each execution of the try-with.
// Each ResumptionCons contains a handler instance (typed as Object).
