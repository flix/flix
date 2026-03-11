enum Color {
    case Red,
    case Green,
    case Blue
}

mod Color {
    pub def isWarm(c: Color): Bool = match c {
        case Red    => true
        case Green  => false
        case Blue   => false
    }
}

def main(): Unit \ IO =
    println(Color.isWarm(Color.Red))
