package dev.flix.runtime.example;

import dev.flix.runtime.*;

public class Def_v {

    public static Result apply() {
        return new Suspension(
                "Con",
                new UseOfConsole() {
                    @Override
                    public Result apply(Console c) {
                        return c.read();
                    }
                },
                new FramesNil(),
                new ResumptionNil()
        );
    }

}
