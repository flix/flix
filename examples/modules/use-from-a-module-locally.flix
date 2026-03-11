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
    use A.Hue

    pub def isCold(c: Color): Bool = {
        use A.isWarm;
        not isWarm(c)
    }

    pub def isRed(c: Hue): Bool = {
        use A.Color.{Red, Green, Blue};
        match c {
            case Red   => true
            case Green => false
            case Blue  => false
        }
    }

}

def main(): Unit \ IO =
    use B.isCold;
    use A.Color.Blue;
    println(isCold(Blue))
