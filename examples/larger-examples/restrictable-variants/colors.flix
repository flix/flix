mod Colors {

    pub restrictable enum Color[i] {
        case Red
        case Green
        case Blue
    }

    // To run this function press the floating "Run" button right below these
    // three lines. This can be done for any function that is `pub` and does not
    // take arguments. (It doesnt not appear until the code is successfully compiled)
    pub def main(): Unit \ IO = {
        // create the color red
        let red = Color.Red;
        println("this is red: ${red}");

        // create the color green with an open type
        let green = open Color.Green;
        println("this is green: ${green}");

        // for the non-general functions we need to open red again
        println("Is red warm? ${isWarm(open_as Color red)}, ${isWarmAlternative(open_as Color red)}, and ${isWarmGeneral(red)}");

        // to compare the two types, we need to open the type of red
        // (green is recreated to avoid propagating typing backwards but it
        // can be reused.)
        println("Are these two colors equal? ${(open_as Color red) == (open Color.Green)}");

        // lets look at the inferred type of isWarm
        let _isWarmLocal = c -> choose c {
            case Color.Red => true
            case Color.Blue => false
        };
        // by hovering on `_isWarmLocal` you should see the type (possibly alpha-renamed):
        //     Color[c28123 & {Blue, Red}] -> Bool
        // by hovering on `c` right after, you should see the type:
        //     Color[c28123 & {Blue, Red}]
        //     Lowerbound: {}
        //     Upperbound: {Blue, Red}
        // This shows the type on the first line, and since its a restrictable
        // variant it also shows the lowerbound and upperbound of the index
        // (only in terms of constants, i.e. bounds that hold for all
        // instantiations of variables).

        // Feel free to experiment some more in this file or move on to the
        // other files

        ()
    }


    pub def isWarm(c: Color[<Color.Red, Color.Blue>]): Bool =
        choose c {
            case Color.Red  => true
            case Color.Blue => false
        }

    pub def isWarmAlternative(c: Color[~~<Color.Green>]): Bool =
        choose c {
            case Color.Red  => true
            case Color.Blue => false
        }

    // The types of isWarm and isWarmAlternative are equivalent.

    // A most general -- and more useful -- type is:

    pub def isWarmGeneral(c: Color[s -- <Color.Green>]): Bool =
        choose c {
            case Color.Red  => true
            case Color.Blue => false
        }

    // Color checking functions

    pub def isRed(c: Color[s]): Bool = choose c {
        case Color.Red   => true
        case Color.Green => false
        case Color.Blue  => false
    }

    pub def isGreen(c: Color[s]): Bool = choose c {
        case Color.Red   => false
        case Color.Green => true
        case Color.Blue  => false
    }

    pub def isBlue(c: Color[s]): Bool = choose c {
        case Color.Red   => false
        case Color.Green => false
        case Color.Blue  => true
    }

}

// Trait Instances

instance ToString[Colors.Color[i]] {
    pub def toString(x: Colors.Color[i]): String = choose x {
        case Colors.Color.Red   => "Red"
        case Colors.Color.Green => "Green"
        case Colors.Color.Blue  => "Blue"
    }
}

instance Eq[Colors.Color[i]] {
    pub def eq(x: Colors.Color[i], y: Colors.Color[i]): Bool = choose x {
        case Colors.Color.Red   => Colors.isRed(y)
        case Colors.Color.Green => Colors.isGreen(y)
        case Colors.Color.Blue  => Colors.isBlue(y)
    }
}
