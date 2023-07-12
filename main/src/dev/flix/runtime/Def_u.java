package dev.flix.runtime;

public class Def_u implements Action {

    @Override
    public int getResult() {
        return 0;
    }

    //    def u(): Int32 \ Con =
    //      let name = do Con.read();
    //      let greetings = "Hello ${name}";
    //      do Con.print(greetings);
    //      String.length(name)
    public Action apply(DefUFrame frame) {
        // locals
        String name;
        String greetings;

        while (true) {
            if (frame.pc == 0) {
                // no local variables to restore.
                // return some kind of suspension
            } else if (frame.pc == 1) {
                // must restore name
                name = frame.name;
                // return suspension for
            } else if (frame.pc == 2) {
                // restore name and greetings
                name = frame.name;
                greetings = frame.greetings;
                // Invoke String.length(name)
            } else {
                throw null;
            }
        }

    }

}

class DefUFrame {
    public int pc;
    public String name;
    public String greetings;
}
