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

    type alias Layout = { indent = Indent, defaultIndentation = DefaultIndentation, flow = Flow }

// TODO: Model as sum of all three. Maybe just record type alias?
/*
    eff Layout {
        Indent,
        DefaultIndentation,
        Flow
    }
*/
// https://effekt-lang.org/docs/casestudies/prettyprinter

}
