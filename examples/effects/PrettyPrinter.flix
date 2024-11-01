/**
 * Example ported from Effekt case study of Pretty Printing.
 */

mod PrettyPrinter {

    enum Direction {
        case Horizontal
        case Vertical
    }

    eff Indent {
        pub def indent(): Int32
    }

    eff DefaultIndentation {
        pub def defaultIndentation(): Int32
    }

    eff Flow {
        pub def flow(): Direction
    }

    type alias Layout = Indent + DefaultIndentation + Flow

    eff Emit {
        pub def emitText(content: String): Unit
        pub def emitNewline(): Unit
    }

    def text(content: String): Unit \ Emit = Emit.emitText(content)

    def newline(): Unit \ Emit = Emit.emitNewline()

    def space(): Unit \ Emit = text(" ")

    def spaces(n: Int32): Unit \ Emit = if (n > 0) text(String.repeat(n, " ")) else ()

    def lineOr(replace: String): Unit \ Flow + Emit + Indent = match Flow.flow() {
        case Direction.Horizontal => text(replace)
        case Direction.Vertical   =>
            newline();
            text(String.repeat(Indent.indent(), " "))
    }

    def line(): Unit \ Flow + Emit + Indent = lineOr(" ")

    def lineBreak(): Unit \ Flow + Emit + Indent = lineOr("")

    def in(doc: Unit -> a \ Layout, n: Int32): a \ DefaultIndentation + Flow =
        try {
            doc()
        } with Indent {
            def indent(k) = k(n)
        }

    def nest(doc: Unit -> a \ Layout, j: Int32): a \ Layout =
        (Indent.indent() + j) |> in(doc)

    def nested(doc: Unit -> a \ Layout): a \ Layout =
        nest(doc, DefaultIndentation.defaultIndentation())

    def inDirection(doc: Unit -> a \ Layout, direction: Direction): a \ Indent + DefaultIndentation =
        try {
            doc()
        } with Flow {
            def flow(k) = k(direction)
        }

    def horizontal(p: Unit -> Unit \ Layout): Unit \ Indent + DefaultIndentation =
        Direction.Horizontal |> inDirection(p)

    def vertical(p: Unit -> Unit \ Layout): Unit \ Indent + DefaultIndentation =
        Direction.Vertical |> inDirection(p)

    eff LayoutChoice {
        pub def choice(): Direction
        pub def fail(): Unit
    }

    type alias Pretty = Emit + Layout + LayoutChoice

    def group(p: Unit -> Unit \ Layout): Unit \ Indent + DefaultIndentation + LayoutChoice =
        (LayoutChoice.choice()) |> inDirection(p)

    def softLine(): Unit \ Indent + DefaultIndentation + LayoutChoice = group(line)

    def softBreak(): Unit \ Indent + DefaultIndentation + LayoutChoice = group(lineBreak)

    // Examples

    def example1(l: List[Int32]): Unit \ Flow + Emit + Indent = {
        text("[");
        foreach(x <- l) {
            text("${x}");
            text(",");
            line()
        };
        text("]")
    }

    def example2(): Unit \ Pretty = {
        group(() -> { text("Hi"); line(); text("you") });
        text("!!!")
    }

    def example3(): Unit \ Pretty = {
        let content = () -> {
            text("this");
            nest(() -> {
                line();
                group(() -> {
                    text("takes");
                    line();
                    text("four")
                })
            }, 9);
            line();
            text("lines")
        };
        group(content)
    }

    pub def main(): Unit \ IO = {
        println("")
    }

}
