package dev.flix.runtime.example;

import dev.flix.runtime.*;

public class Def_v {

    public static Result apply() {
        return new Suspension(
                "Con",
                new EffectCall() {
                    @Override
                    public Result apply(Handler h, Resumption r) {
                        Console c = (Console) h;
                        return c.read(r);
                    }
                },
                new FramesNil(),
                new ResumptionNil()
        );
    }

}
