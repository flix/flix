package dev.flix.runtime;

public class Def_u {

    //    def u(): Int32 \ Con =
    //      let name = do Con.read();
    //      let greetings = "Hello ${name}";
    //      do Con.print(greetings);
    //      String.length(name)
    public DefUFrame apply(DefUFrame frame) {
        // locals
        String name;
        String greetings;

        while (true) {
            if (frame.pc == 0) {
                // no local variables to restore.
                return new DefUFrame(1, "Con.read", null, null);
            } else if (frame.pc == 1) {
                // must restore name
                name = frame.name;
                greetings = "Hello " + name;
                return new DefUFrame(2, "Con.print", name, greetings);
            } else if (frame.pc == 2) {
                // restore name and greetings
                name = frame.name;
                greetings = frame.greetings;
                var tmp = greetings.length();
                var f = new DefUFrame(0, null, null, null);
                f.result = tmp;
                return f;
            }
        }

    }

}

class DefUFrame {
    public int pc;
    public String op;
    public String name;
    public String greetings;
    public int result;

    public DefUFrame(int pc, String op, String name, String greetings) {
        this.pc = pc;
        this.op = op;
        this.name = name;
        this.greetings = greetings;
    }
}
