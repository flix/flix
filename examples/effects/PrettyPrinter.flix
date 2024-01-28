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

    def text(content: String): Unit \ Emit = do Emit.emitText(content)

    def newline(): Unit \ Emit = do Emit.emitNewline()

    def space(): Unit \ Emit = text(" ")

    def spaces(n: Int32): Unit \ Emit = if (n > 0) text(String.repeat(n, " ")) else ()

    def lineOr(replace: String): Unit \ Flow + Emit + Indent = match do Flow.flow() {
        case Direction.Horizontal => text(replace)
        case Direction.Vertical   =>
            newline();
            text(String.repeat(do Indent.indent(), " "))
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
        (do Indent.indent() + j) |> in(doc)

    def nested(doc: Unit -> a \ Layout): a \ Layout =
        nest(doc, do DefaultIndentation.defaultIndentation())

    def inDirection(doc: Unit -> a \ Layout, direction: Direction): a \ Indent + DefaultIndentation =
        try {
            doc()
        } with Flow {
            def flow(k) = k(direction)
        }

    def horizontal(p: Unit -> Unit \ Layout): Unit \ Layout =
        Direction.Horizontal |> in(p)

    def vertical(p: Unit -> Unit \ Layout): Unit \ Layout =
        Direction.Vertical |> in(p)

    eff LayoutChoice[a] {
        pub def choice(): Direction
        pub def fail(): a
    }

    type alias Pretty = Emit + Layout + LayoutChoice

    def group(p: Unit -> Unit \ Layout): Unit \ Layout + LayoutChoice =
        (do Choice.choice()) |> in(p)

    def softLine(): Unit \ Pretty = group(line)

    def softBreak(): Unit \ Pretty = group(lineBreak)

    // Examples

    pub def main(): Unit \ IO = {
        println("")
    }

}
