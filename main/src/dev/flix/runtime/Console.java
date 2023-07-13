package dev.flix.runtime;

public interface Console {
    // Result read(Locals_?, Kont?);
    Result read();

    Result print(String s);

}

class ConsoleHandler17 implements Console {
    public Result read() {
        // TODO: Much later, resume with say 21.
        return Done.mkInt32(21);
    }

    public Result print(String s) {
        return Done.mkInt32(42);
    }

}

// and then we can create objects/instances for each execution of the try-with.
// Each ResumptionCons contains a handler instance (typed as Object).
