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

    def in(doc: Unit -> a \ Indent + DefaultIndentation + Flow, n: Int32): a \ DefaultIndentation + Flow =
        try {
            doc()
        } with Indent {
            def indent(k) = k(n)
        }
}
