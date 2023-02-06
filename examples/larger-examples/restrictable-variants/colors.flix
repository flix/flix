restrictable enum Color[i] {
    case Red
    case Green
    case Blue
}

def isWarm1(c: Color[<Color.Red, Color.Blue>]): Bool =
    choose c {
        case Color.Red  => true
        case Color.Blue => false
    }

def isWarm2(c: Color[~~<Color.Green>]): Bool =
    choose c {
        case Color.Red  => true
        case Color.Blue => false
    }

// The types of isWarm1 and isWarm2 are equivalent.

// A more general, and useful, type is:

def isWarm3(c: Color[s -- <Color.Green>]): Bool =
    choose c {
        case Color.Red  => true
        case Color.Blue => false
    }
