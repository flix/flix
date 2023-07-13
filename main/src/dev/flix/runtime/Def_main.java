package dev.flix.runtime;

import dev.flix.runtime.example.Def_u;
import dev.flix.runtime.example.Locals_u;

public class Def_main {

    public static void main(String[] args) {
        Result vResult = apply(null);
        while (vResult instanceof Thunk) {
            vResult = ((Thunk) vResult).apply();
        }
        if (vResult instanceof Suspension) {
            throw new RuntimeException("Unhandled effect");
        }

        System.out.println(vResult);
    }

    public static Result apply(Locals_main locals) {
        ConsoleHandler17 handler = new ConsoleHandler17();
        return Handler.installHandler("Con", handler, new FramesNil(), new Thunk() {
            @Override
            public Result apply() {
                return Def_u.apply(new Locals_u(0, null, null), null);
            }
        });
    }

}

class Locals_main {
}
