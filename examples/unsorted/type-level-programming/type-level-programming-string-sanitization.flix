/// We can use a type-level Booleand to express a type-safe API
/// for working with santizied and unsatized inputs.
enum Input[_sanitized: Bool] {
    case Input(String)
}

/// We introduce two handy type aliases:
type alias Sanitized = true
type alias Unsantized = false

/// We can model a sensitive operation that requires a sanitized input.
def sensitive(_i: Input[Sanitized]): Unit = ()

/// A function that performs string sanitization.
def sanitize(i: Input[Unsantized]): Input[Sanitized] = match i {
    case Input.Input(s) => Input.Input(String.toLowerCase(s))
}

/// We can concatenate two inputs. The result is santized if
/// both inputs are sanitized.
def concatenate(i1: Input[s1], i2: Input[s2]): Input[s1 and s2] =
    match (i1, i2) {
        case (Input.Input(s1), Input.Input(s2)) => Input.Input(s1 + s2)
    }
