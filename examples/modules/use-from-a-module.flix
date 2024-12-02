mod A {
    pub enum Color {
        case Red, Green, Blue
    }

    pub type alias Hue = Color

    pub def isWarm(c: Color): Bool =
        match c {
            case Color.Red    => true
            case Color.Green  => false
            case Color.Blue   => false
        }
}

mod B {
    use A.Color
    use A.Color.{Red, Green, Blue}
    use A.Hue
    use A.isWarm

    pub def isCold(c: Color): Bool = not A.isWarm(c)

    pub def isRed(c: Hue): Bool = match c {
        case Red   => true
        case Green => false
        case Blue  => false
    }

}

def main(): Unit \ IO = 
    println(B.isCold(A.Color.Blue))
