enum Input[_sanitized: Bool] {
  case Str(String)
}

type alias Sanitized = true
type alias Unsantized = false

//sanitize a string in order to ensure security
def sanitize(_: String): String = {
  ???//add sanitization code here and return intended string
}

//make a new sanitized Input from a given string
def newSanitizedInput(s: String): Input[Sanitized] = {
  Input.Str(sanitize(s))
}

//sanitize an Input(can be done on an already sanitized Input)
def sanitizeInput(input: Input[_]): Input[Sanitized] = match input {
  case Input.Str(s) => Input.Str(sanitize(s))
}

//Apply a function to a sanitized Input. This is safe because Flix's type system enforces
//that the given Input is sanitized, or else a type error transpires at compile time
def useSanitizedStr(input: Input[Sanitized], f: String -> Unit): Unit = match input {
  case Input.Str(s) => f(s)
}